-module(eunit_macro_expander).

-compile(export_all).

-include("../include/tracing.hrl").


%% File is a String, "Module.erl".

dynamic_file(File) -> file(File, dynamic).
static_file(File) -> file(File, static).

file(File, Mode) ->
    {ok, Trees} = epp_dodger:parse_file(File),
    Tree = erl_syntax:form_list(Trees),
    NewTree = transform_tree(Mode, Tree),
    ListElements = erl_syntax:form_list_elements(NewTree),
    {Head,Body} = split(Mode, ListElements),
    {Macros1, Macros2} = create_macros(Mode),
    MacroTree = erl_syntax:form_list(Head++Macros1++Macros2++Body),
    {filename:basename(File),erl_prettypr:format(MacroTree)}.

transform_tree(dynamic, Tree) -> erl_syntax_lib:map(fun transformer/1,Tree);
transform_tree(static, Tree) -> Tree.

create_macros(dynamic) ->
    Macros1 = [makeMacroTree(MName,test_negative) || MName <- [assertError,
						             assertExit,
						             assertException,
						             assertThrow]],
    Macros2 = [makeMacroTree(MName,negative_wrap) || MName <- ['_assertError',
						             '_assertExit',
						             '_assertException',
						             '_assertThrow']],
    {Macros1, Macros2};

create_macros(static) ->
    Macros1 = [makeMacroTree_static(MName) || MName <- ['_assertMatch', assertMatch,
							'_assertError', assertError,
							'_assertExit', assertExit,
							'_assertException', assertException,
							'_assertThrow', assertThrow]],
    Macros2 = [erl_syntax:attribute(erl_syntax:variable(define),
				    [erl_syntax:variable("EUNIT_HRL"), erl_syntax:atom('true')])],
    {Macros1, Macros2}.
    

%% Transforms a tree, doing two things
%%  - replaces macros of the form assertXXX
%%    by assertXXXTrace, when these are negative
%%    e.g. assertError,
%%  - wraps the bodies of tests and test objects inside
%%    calls to test_wrap and test__wrap (from tracing).

transformer(Tree) ->
    case erl_syntax:type(Tree) of
	macro ->
	    MName = erl_syntax:macro_name(Tree),
	    MacroAtomName =
		case  erl_syntax:type(MName) of
		    atom -> erl_syntax:atom_value(MName);
		    variable -> erl_syntax:variable_name(MName)
		end,
            case MacroAtomName of
		assertError ->
		   erl_syntax:macro(erl_syntax:atom(assertErrorTrace),
				     erl_syntax:macro_arguments(Tree));
		'_assertError' ->
		   erl_syntax:macro(erl_syntax:atom('_assertErrorTrace'),
				     erl_syntax:macro_arguments(Tree));
		assertExit ->
		   erl_syntax:macro(erl_syntax:atom(assertExitTrace),
				     erl_syntax:macro_arguments(Tree));
		'_assertExit' ->
		   erl_syntax:macro(erl_syntax:atom('_assertExitTrace'),
				     erl_syntax:macro_arguments(Tree));
		assertException ->
		   erl_syntax:macro(erl_syntax:atom(assertExceptionTrace),
				     erl_syntax:macro_arguments(Tree));
		'_assertException' ->
		   erl_syntax:macro(erl_syntax:atom('_assertExceptionTrace'),
				     erl_syntax:macro_arguments(Tree));
		assertThrow ->
		   erl_syntax:macro(erl_syntax:atom(assertThrowTrace),
				     erl_syntax:macro_arguments(Tree));
		'_assertThrow' ->
		   erl_syntax:macro(erl_syntax:atom('_assertThrowTrace'),
				     erl_syntax:macro_arguments(Tree));
		_ -> Tree
	    end;
	function ->
	    FName = erl_syntax:function_name(Tree),
            Arity = erl_syntax:function_arity(Tree),
	    case erl_syntax:type(FName) of
	 	atom ->
	 	    case is_testFun(erl_syntax:atom_value(FName), Arity) of
	 		test ->
	 		    wrap_test_function(Tree,FName,?tracing,test_wrap);
	 		test_object ->
	 		    wrap_test__function(Tree,FName,?tracing,test__wrap);
	 		_ -> Tree
	 	    end;
	 	_ -> Tree
	     end;
	_ -> Tree
    end.


%% Builds the macro definition
%%  -define(NewName(X,Y),?OldName(X,Y)).

makeMacroTree(OldName) ->
    makeMacroTree(OldName,test_negative).

makeMacroTree(OldName,Wrapper) ->
    NewName = list_to_atom(lists:concat([OldName,"Trace"])),
    X = erl_syntax:variable("X"),
    Y = erl_syntax:variable("Y"),
    LHS = erl_syntax:application(erl_syntax:atom(NewName),[X,Y]),
    ModName = erl_syntax:atom(?tracing),
    FunName = erl_syntax:atom(Wrapper),
    MacApp = erl_syntax:macro(erl_syntax:atom(OldName),[X,Y]),
    Assert = erl_syntax:macro(erl_syntax:macro(X)),
    RHS = erl_syntax:application(erl_syntax:module_qualifier(ModName,FunName),
                                 [Assert,MacApp]),
    erl_syntax:attribute(erl_syntax:atom(define),[LHS,RHS]).

makeMacroTree_static(assertException) ->
    makeMacroTree_static(assertException, 3, fsm_eunit_parser_negative);
makeMacroTree_static('_assertException') ->
    makeMacroTree_static('_assertException', 3, fsm_eunit_parser_negative);
makeMacroTree_static(assertMatch) ->
    makeMacroTree_static(assertMatch, 2, fsm_eunit_parser_positive);
makeMacroTree_static('_assertMatch') ->
    makeMacroTree_static('_assertMatch', 2, fsm_eunit_parser_positive);
makeMacroTree_static(Other) -> makeMacroTree_static(Other, 2, fsm_eunit_parser_negative).

makeMacroTree_static(MacroName, N, Sig) ->
    [Trace|ExtraParams] = genParameters(N),
    Parameters = ExtraParams++[Trace],
    LHS = erl_syntax:application(erl_syntax:atom(MacroName), Parameters),
    RHS = case Sig of
	      fsm_eunit_parser_positive -> Trace;
	      _ -> erl_syntax:tuple([erl_syntax:atom(Sig), Trace])
	  end,
    erl_syntax:attribute(erl_syntax:atom(define), [LHS, RHS]).

genParameters(N) -> [erl_syntax:variable("Trace")|genParameters(N, 1)].
genParameters(1, _) -> [];
genParameters(N, L) when N > 1 -> [erl_syntax:variable("P"++integer_to_list(L))|
				     genParameters(N - 1, L + 1)].

%% Splits the form list after the attributes
%% (and so before the functions).
split(dynamic, Fs) ->
    lists:splitwith(fun (F) ->
			     erl_syntax:type(F) == attribute end,
		    Fs);
%% Splits the form list after the attributes
%% or before the first include or include_lib
%% directive if it exists
split(static, Fs) ->
    lists:splitwith(fun check_if_not_include/1, Fs).
check_if_not_include(F) ->
    case (erl_syntax:type(F)) of
	attribute -> Name = erl_syntax:attribute_name(F),
		     case erl_syntax:type(Name) of
			 atom -> case erl_syntax:atom_value(Name) of
				     include -> false;
				     include_lib -> false;
				     _ -> true
				 end;
			 _ -> true
		     end;
	_ -> false
    end.


%% Is an atom a test: ..._test,
%% a test object: ..._test_,
%% or not?

is_testFun(Atom, Arity) ->    
    String = lists:reverse(atom_to_list(Atom)),
    case {String, Arity} of
	{"_tset_"++ _, 0} ->
	    test_object;
	{"tset_"++ _, 0} ->
	    test;
	_ ->
	   no_test
    end.

wrap_test_function(Tree,FName,Module,Function) ->
    [Clause] = erl_syntax:function_clauses(Tree),
    Body = erl_syntax:clause_body(Clause),
    Fun = erl_syntax:fun_expr([erl_syntax:clause([],none,Body)]),
    ModName = erl_syntax:atom(Module),
    FunName = erl_syntax:atom(Function),
    RHS = erl_syntax:application(erl_syntax:module_qualifier(ModName,FunName),[Fun]),
    erl_syntax:function(FName,[erl_syntax:clause([],none,[RHS])]).

wrap_test__function(Tree,FName,Module,Function) ->
    [Clause] = erl_syntax:function_clauses(Tree),
    Body = erl_syntax:clause_body(Clause),
    ModName = erl_syntax:atom(Module),
    FunName = erl_syntax:atom(Function),
    RHS = erl_syntax:application(erl_syntax:module_qualifier(ModName,FunName),
                                 [erl_syntax:block_expr(Body)]),
    erl_syntax:function(FName,[erl_syntax:clause([],none,[RHS])]).

