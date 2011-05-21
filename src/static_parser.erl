%%%-------------------------------------------------------------------
%%% File    : static_parser.erl
%%% Author  : Pablo Lamela <lamela@student.chalmers.se>
%%% Description : Library that parses eunit modules to extract traces
%%%
%%% Created : 21 Jan 2011 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(static_parser).

%% API
-export([parse_file/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: parse_file(File)
%% Description: Tries to parse the File without executing it
%%--------------------------------------------------------------------

parse_file(File) ->
    Patterns = pp_file(File),
    Functions = lists:map(fun prepare_function/1,
			  get_function_names_and_contents(Patterns)),
    ImportedFunctions = lists:flatten(lists:map(fun get_imported_functions/1,
						Patterns)),
    FindFunc = gen_FindFunc(get_module_name(Patterns),ImportedFunctions,
			    Functions),
    Tests = lists:filter(fun (X) -> case X of
					{_,_,_,normal} -> true;
					{_,_,_,generator} -> true;
					_ -> false
				    end end, Functions),
    divideTests(lists:map(gen_getTraces(FindFunc), Tests)).

%%====================================================================
%% Internal functions
%%====================================================================

pp_file(File) -> case epp:parse_file(File, ["../include"], []) of
		     {ok, Parsed} -> Parsed;
		     {error, _} = Error -> throw(Error)
		 end.

fetch_function_name({attribute,_,_,_}) -> [];
fetch_function_name({function,_,Name,Arity,Content}) -> {{Name,Arity},Content};
fetch_function_name({eof,_}) -> [];
fetch_function_name({error, _} = Error) -> throw(Error).

get_function_names_and_contents(ParsedTokens) ->
    lists:flatten(lists:map(fun fetch_function_name/1, ParsedTokens)).

get_imported_functions({attribute,_,import,{Mod,List}}) ->
    lists:map(fun (X) -> {Mod, X} end, List);
get_imported_functions({attribute,_,_,_}) -> [];
get_imported_functions({function,_,_,_,_}) -> [];
get_imported_functions({eof,_}) -> [];
get_imported_functions({error, _} = Error) -> throw(Error).

get_module_name([{attribute,_,module,ModuleName}|_]) -> ModuleName;
get_module_name([_|Tail]) -> get_module_name(Tail);
get_module_name([]) -> throw({error, noModuleName}).

prepare_function({{Name,N},Content}) ->
	{Name,N,Content,get_type(Name)}.

get_type(Name) when is_atom(Name) -> get_type(atom_to_list(Name));
get_type("test") -> normal;
get_type("test_") -> generator;
get_type([_|Tail]) -> get_type(Tail);
get_type([]) -> aux.

gen_getTraces(FindFunc) ->
    fun (Test) -> get_traces(Test, FindFunc) end.

isNeg([]) -> false;
isNeg([neg]) -> true;
isNeg([_|T]) -> isNeg(T);
isNeg(_) -> false.

simplify_call_list([{tuple,_,[{atom,_,fsm_eunit_parser_negative},
			      Call]}|_], FindFunc) ->
    Result = resolve_call(Call, FindFunc),
    case isNeg(Result) of
	true -> [Result];
	false -> [Result, neg]
    end;
simplify_call_list([{call,_,_,_} = Call|Tail], FindFunc) ->
    Result = resolve_call(Call, FindFunc),
    case isNeg(Result) of
	true -> [Result];
	false -> [Result|simplify_call_list(Tail, FindFunc)]
    end;
simplify_call_list([_ = Static|Tail], FindFunc) ->
    static_evaluator(Static),
    simplify_call_list(Tail, FindFunc);
simplify_call_list([], _) -> [pos].

resolve_call({call,_,{remote,_,Module,Name},Args}, _) ->
    {static_evaluator(Module), static_evaluator(Name),
     lists:map(fun static_evaluator/1, Args)};
resolve_call({call,_,Name,Args}, FindFunc) ->
    case FindFunc(static_evaluator(Name),
		  lists:map(fun static_evaluator/1,Args)) of
	{replace_aux, Func} ->
	    get_traces({'call', 0, Func, normal}, FindFunc);
	Trace -> Trace
    end.

resolve_fun({'fun',_,{clauses,Func}}, FindFunc) ->
    get_traces({'fun', 0, Func, normal}, FindFunc);
resolve_fun({'fun',L,{function,Name,0}}, FindFunc) ->
    resolve_call({call,L,Name,[]}, FindFunc);
resolve_fun({'fun',L,{function,Module,Name,0}}, FindFunc) ->
    resolve_call({call,L,{remote,L,Module,Name},[]}, FindFunc).

gen_FindFunc(ModuleName, IF, Functions) ->
    fun (Name, Args) -> find_func(Name, Args, IF, Functions, ModuleName) end.

get_traces({_,0,[{clause,_,[],[],List}],normal}, FindFunc) ->
    lists:flatten(simplify_call_list(List, FindFunc));
get_traces({_,0,[{clause,_,[],[],List}],generator}, FindFunc) ->
    replace_generators(List, FindFunc);
get_traces(_,_) -> [].

removeResult([pos]) -> [];
removeResult([H|T]) -> [H|removeResult(T)].

combine(SetUp) -> fun (X) -> removeResult(SetUp) ++ X end.

replace_generators([{tuple, _, [{atom, _, setup}, SetUp, Tests]}], FindFunc) ->
    removeResult(resolve_fun(SetUp, FindFunc)) ++ resolve_fun(Tests, FindFunc);
replace_generators([{tuple, L, [{atom, L2, setup}, SetUp, _CleanUp, Tests]}],
		   FindFunc) ->
    replace_generators([{tuple, L, [{atom, L2, setup},
				    SetUp, Tests]}], FindFunc);
replace_generators([{tuple, _, [{atom, _, inorder}, Tests]}], FindFunc) ->
    lists:map(lists:map(fun (X) -> resolve_fun(X, FindFunc) end,
			static_list_evaluator(Tests)));
replace_generators([{tuple, _, [{atom, _, foreach}, SetUp, Tests]}],
		   FindFunc) ->
    lists:map(combine(resolve_fun(SetUp,FindFunc)),
	      lists:map(fun (X) -> resolve_fun(X, FindFunc) end,
			static_list_evaluator(Tests)));
replace_generators([{tuple, L, [{atom, L2, foreach},
				SetUp, _CleanUp, Tests]}], FindFunc) ->
    replace_generators([{tuple, L, [{atom, L2, foreach},
				    SetUp, Tests]}], FindFunc).

find_func(Name, [], _IF, [{Name, 0, Func, aux}|_], _MN) -> {replace_aux, Func};
find_func(Name, Arguments, _IF, [{Name, Arity, _Func, aux}|_], ModuleName)
  when erlang:length(Arguments) =:= Arity -> {ModuleName, Name, Arguments};
find_func(Name, Arguments, _IF, [{Name, Arity, Func, normal}|_], ModuleName)
  when erlang:length(Arguments) =:= Arity -> {ModuleName, Name, Func};
find_func(Name, Arguments, _IF, [{Name, Arity, Func, generator}|_],
	  ModuleName) when erlang:length(Arguments) =:= Arity ->
    {ModuleName, Name, Func};
find_func(N, A, I, [_|Tail], MN) -> find_func(N, A, I, Tail, MN);
find_func(Name, Arguments, [{Module, {Name, Arity}}|_], [], _)
  when erlang:length(Arguments) =:= Arity -> {Module, Name, Arguments};
find_func(N, A, [_|Tail], [], MN) -> find_func(N, A, Tail, [], MN);
find_func(Name, Arity, [], [], _MN) -> throw({error, {func_not_found, Name, Arity}}).

static_evaluator({integer,_,I}) -> I;
static_evaluator({float,_,F}) -> F;
static_evaluator({string,_,S}) -> S;
static_evaluator({atom,_,A}) -> A;
static_evaluator({tuple,_,List}) -> list_to_tuple(List);
static_evaluator({nil,_}) -> [];
static_evaluator({cons,_,H,T}) -> [static_evaluator(H)|static_evaluator(T)];
static_evaluator({bin,_,[]}) -> <<>>;
% Couldn't find a way to implement the TypeSpecifierList in binaries
static_evaluator({bin,L,[{bin_element,_,P,Size,default}|Tail]}) ->
    <<(static_evaluator(P)):(static_evaluator(Size)),
      (static_evaluator({bin,L,Tail}))/binary>>;
static_evaluator({op,_,'+',Val}) -> +(static_evaluator(Val));
static_evaluator({op,_,'-',Val}) -> -(static_evaluator(Val));
static_evaluator({op,_,'bnot',Val}) -> bnot(static_evaluator(Val)).

static_list_evaluator({nil,_}) -> [];
static_list_evaluator({cons,_,H,T}) -> [H|static_list_evaluator(T)].

divideTests(L) -> divideTests([], [], [], lists:flatten(L)).
divideTests(Pos, Neg, [], []) -> {lists:reverse(Pos), lists:reverse(Neg)};
divideTests(Pos, Neg, Actual, [pos|Tail]) ->
    divideTests([lists:reverse(Actual)|Pos], Neg, [], Tail);
divideTests(Pos, Neg, Actual, [neg|Tail]) ->
    divideTests(Pos, [lists:reverse(Actual)|Neg], [], Tail);
divideTests(Pos, Neg, Actual, [H|Tail]) ->
    divideTests(Pos, Neg, [H|Actual], Tail).
