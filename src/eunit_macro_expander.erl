-module(eunit_macro_expander).

-compile(export_all).

-include("../include/tracing.hrl").


%% File is a String, "Module.erl".

file(File) ->
    {ok, Trees} = epp_dodger:parse_file(File),
    Macros1 = [makeMacroTree(MName,test_negative) || MName <- [assertError,
						             assertExit,
						             assertException,
						             assertThrow]],
    Macros2 = [makeMacroTree(MName,negative_wrap) || MName <- ['_assertError',
						             '_assertExit',
						             '_assertException',
						             '_assertThrow']],
    Tree = erl_syntax:form_list(Trees),
    NewTree = erl_syntax_lib:map(fun transformer/1,Tree),
    {Head,Body} = split(erl_syntax:form_list_elements(NewTree)),
    MacroTree = erl_syntax:form_list(Head++Macros1++Macros2++Body),
    {filename:basename(File),erl_prettypr:format(MacroTree)}.

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
	    case erl_syntax:type(FName) of
	 	atom ->
	 	    case is_testFun(erl_syntax:atom_value(FName)) of
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
    RHS = erl_syntax:application(erl_syntax:module_qualifier(ModName,FunName),[MacApp]),
    erl_syntax:attribute(erl_syntax:atom(define),[LHS,RHS]).

%% Splits the form list after the attributes
%% (and so before the functions).

split(Fs) ->
    lists:splitwith(fun (F) ->
			     erl_syntax:type(F) == attribute end,
		    Fs).

%% Is an atom a test: ..._test,
%% a test object: ..._test_,
%% or not?

is_testFun(Atom) ->    
    String = lists:reverse(atom_to_list(Atom)),
    case String of
	"_tset_"++ _ ->
	    test_object;
	"tset_"++ _ ->
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
    [Body] = erl_syntax:clause_body(Clause),
    ModName = erl_syntax:atom(Module),
    FunName = erl_syntax:atom(Function),
    RHS = erl_syntax:application(erl_syntax:module_qualifier(ModName,FunName),[Body]),
    erl_syntax:function(FName,[erl_syntax:clause([],none,[RHS])]).

