%%%-------------------------------------------------------------------
%%% File    : trace_runner.erl
%%% Author  : Simon Thompson
%%% Description : Module to collect traces by running the tests against
%%% the SUT.
%%%
%%% Created : Feb 2011 by Simon Thompson
%%% Modified: May 2011 by Thomas Arts
%%% Modified: Aug 2011 by Simon Thompson
%%%-------------------------------------------------------------------

-module(trace_runner).

%% Runs the EUnit test functions from Module:test() (i.e. in Module.erl
%% or Module_tests.erl) with tracing as provided by eunit_tracing.erl

-export([start/1,           %% functions to derive pair of trace sets {Pos,Neg}
	 start/2,           %% 2 argument version takes list of API functions.
	 consistent/1]).    %% check whether a pair of trace sets is consistent

-export([test1/0,test2/0,   %% for testing
	api/0]).

-export([tester/2,          %% exported because spawned or used in HOFs.
	 make_titems/1,
	 cleanup_item/2,
	 push_posneg/2]).

%% Imports for the definition of the titem record type.

-include("../include/visualize.hrl").

%% TO DO:
%%  1. Move consistent to later in the tool chain, or remove entirely.
%%     Checking over traces here won't behave as expected, since line
%%     number information is included in the titems.

%%%-------------------------------------------------------------------
%%
%% Top-level function.
%% Returns the traces as a pair of lists: positive and negative traces.
%%
%%%-------------------------------------------------------------------

start(Module) ->
    start(Module,none).

start(Module,API) ->
    %% enable tracing for the module
    Stem = stem(Module),
    eunit_tracing:t(Stem),
    
    %% perform the Eunit tests, and log the results.
    spawn(?MODULE, tester, [Module, self()]),
    Msgs = loop([]),
    %%%% io:format("~p~n~nlines: ~p~n~n",[Msgs,length(Msgs)]),
    
    %% Remove EUnit specific entries in the log
    MsgsE = [ Msg || Msg<-Msgs, check_eunit(Module,Msg) ],
    %%%% io:format("~p~n~nlines: ~p~n~n",[MsgsE,length(MsgsE)]),

    %% Filter out only those calls that are in the API. In the one argument
    %% version, signalled by the API list none, once test is opened in Pid N,
    %% discard all log entries from other processes.

    %% In the two argument version, use the API argument and filter just those calls.

    case API of
	none ->
	    Msgs1 = remove_alien_pids(MsgsE);
	_API ->
	    Msgs1 = remove_non_API(MsgsE,Stem,API)
    end,
    %%%% io:format("~p~n~nlines: ~p~n~n",[Msgs1,length(Msgs1)]),
    
    %% Remove nested calls to functions in Module.
    Msgs2 = remove_nested_calls(Stem,Msgs1),
    %%%% io:format("~p~n~nlines: ~p~n~n",[Msgs2,length(Msgs2)]),
    
    %% Discard Pid information and atoms.
    Calls = lists:foldr(fun cleanup_item/2,[],Msgs2),
    %%%% io:format("~p~n~n",[Calls]),

    %% Parse and flatten the (deep) list into a list of lists of log items.
    Structs = parse(Calls),
    Traces = flatten_struct(Structs),
    %%%% io:format("~p~n~n",[Traces]),

    %% Accumulate log items together to give titems.
    Titems = lists:map(fun make_titems/1,Traces),
    %%%% io:format("~p~n~n",[Titems]),
    
    %% Separate positve and negative traces.
    {Pos,Neg} = lists:foldr(fun push_posneg/2, {[], []}, Titems),
    io:format("~p~n",[{Pos, Neg}]),
    {Pos,Neg}.

%%%-------------------------------------------------------------------
%%
%% Principal functions used in defining start/1.
%%
%%%-------------------------------------------------------------------

%% Testing process: runs Module:test()
%% and then tells Pid it has finished.

%% The one second pause is to ensure that
%% all trace messages are collected; without this
%% some carry over to the next call. 

tester(Module,Pid) ->
    Module:test(),
    receive
    after 1000 ->
	    ok
    end,
    Pid ! finished.

%% Loop to collect messages in a list
%% until the finished message is received.

loop(Msgs) ->
    receive
	finished ->
	      lists:reverse(Msgs);
	Msg ->
	    loop([Msg|Msgs])
    end.

%% Is an item Eunit specific? Return false iff so.

check_eunit(Module,M) ->
    case M of
	{_,_,call,{Module,module_info,_}} -> false;
	{_,_,call,{Module,test,_}} -> false;
	{_,_,call,{_,_,_}} -> true;
	{_,_,return_from,{Module,module_info,_},_} -> false;
	{_,_,return_from,{Module,test,_},_} -> false;
	_ -> true
    end.


%% Remove functions from the SUT which are not in the API.

%% In general the API will be a subset of the exports; can extract
%% this automaotically if (and only if) the export is suitably commmented.

remove_non_API([{trace,_,call,{M,F,_}}=Item|Msgs],Module,API) ->
    case lists:member(F,API) orelse M=/=Module of
	true ->
	    [Item|remove_non_API(Msgs,Module,API)];
	false ->
	    remove_non_API(Msgs,Module,API)
    end;

remove_non_API([{trace,_,return_from,{M,F,_},_}=Item|Msgs],Module,API) ->
    case lists:member(F,API) orelse M=/=Module of
	true ->
	    [Item|remove_non_API(Msgs,Module,API)];
	false ->
	    remove_non_API(Msgs,Module,API)
    end;

remove_non_API([],_,_) -> [].

%% When a test is opened in Pid N, discard all trace entries from other processes,

%% That is, when in a test opened with Pid X, then throw away all
%% trace items with Pid /=X until hit close of the corresponding test.

remove_alien_pids(Msgs) ->
    remove_alien_pids(Msgs,[]).

remove_alien_pids([{trace,Pid,call,{?tracing,open,_}}=Item|Msgs],Pids) ->
    [Item|remove_alien_pids(Msgs,[Pid|Pids])];

remove_alien_pids([{trace,Pid,call,{?tracing,close,_}}=Item|Msgs],[Pid|Pids]) ->
    [Item|remove_alien_pids(Msgs,Pids)];

remove_alien_pids([{trace,Pid,call,{_,_,_}}=Item|Msgs],[Pid|_]=Pids) ->
    [Item|remove_alien_pids(Msgs,Pids)];

remove_alien_pids([{trace,PidX,call,{_,_,_}}|Msgs],[Pid|_]=Pids)
  when PidX =/= Pid ->
    remove_alien_pids(Msgs,Pids);

remove_alien_pids([{trace,Pid,return_from,{_,_,_},_}=Item|Msgs],[Pid|_]=Pids) ->
    [Item|remove_alien_pids(Msgs,Pids)];

remove_alien_pids([{trace,PidX,return_from,{_,_,_},_}|Msgs],[Pid|_]=Pids)
  when PidX =/= Pid ->
    remove_alien_pids(Msgs,Pids);

remove_alien_pids([],_) -> [].

%% Removing nested calls to functions from Module.

%% Problem: can't be sure that every function call has a corresponding return.
%% In the no return case, there should be a corresponding negative test indicator

remove_nested_calls(Module,Msgs) ->
    remove_nested_calls(Module,Msgs,no_removal).

remove_nested_calls(Module,[{trace,Pid,call,{Module,Fun,_}}=Item|Msgs],no_removal) ->
    [Item|remove_nested_calls(Module,Msgs,{Pid,Fun})];

remove_nested_calls(Module,[Item|Msgs],no_removal) ->
    [Item|remove_nested_calls(Module,Msgs,no_removal)];

remove_nested_calls(Module,[{trace,Pid,return_from,{Module,Fun,_},_}=Item|Msgs],{Pid,Fun}) ->
    [Item|remove_nested_calls(Module,Msgs,no_removal)];

remove_nested_calls(Module,[{trace,Pid,call,{?tracing,test_negative,_}}=Item|Msgs],{Pid,_Fun}) ->
    [Item|remove_nested_calls(Module,Msgs,no_removal)];

remove_nested_calls(Module,[_Item|Msgs],{Pid,Fun}) ->
    remove_nested_calls(Module,Msgs,{Pid,Fun});

remove_nested_calls(_Module,[],_) ->
    [].

%% Cleanup an item, removing Pids and atoms:

cleanup_item(M,Cs) ->
    case M of
	{_,_,call,Call} -> [Call|Cs];
	{_,_,return_from,MFA,Res} -> [{MFA,Res}|Cs];
	_ -> Cs
    end.

%% Parses a list of messages into a nested format reflecting
%% the structures in the test descriptions.
%% Assumes that the list is well formed - start/end messages
%% are properly matched, and so parse by deterministic recursive
%% descent.

parse(Trace) ->
    parse(Trace,[]).

parse([_|_]=Trace,Tests) ->
    {Test,Rest} = parse_test(Trace),
    parse(Rest,[Test|Tests]);
parse([],Tests) ->
    lists:reverse(Tests).
    
parse_test([{?tracing,open,Mode}|R1]) ->
    {Elems,R2} = elems(R1),
    [{?tracing,close,Mode}|R3]   = R2,
    {{hd(Mode),Elems},R3}.

elems([{?tracing,close,_}|_R]=In) -> {[],In};
elems([{?tracing,open,_}|_R]=In) ->
    {Elem,R2} = parse_test(In),
    {Elems,R3} = elems(R2),
    {[Elem|Elems],R3};
elems([X|R]) ->
    {Elems,R2} = elems(R),
    {[X|Elems],R2};
elems(_) -> 'EXIT'.


%% Flatten structure - coming from parse - into a single list of traces.
%% Result is a list of traces (i.e. a list of lists of items).

flatten_struct(Struct) ->
     lists:concat(lists:map(fun flatten_test_desc/1,Struct)).

flatten_test_desc({test,Trace}) ->
    [Trace];

flatten_test_desc({inparallel,Trace}) ->
   lists:concat(lists:map(fun flatten_test_desc/1,Trace));

flatten_test_desc({inorder,Trace}) ->
   [join(lists:map(fun flatten_test_desc/1, Trace))];

flatten_test_desc({setup,Trace}) ->
    [join(lists:map(fun flatten_test_desc/1, Trace))];

flatten_test_desc({foreach,Trace}) ->
    lists:concat(lists:map(fun flatten_test_desc/1,Trace));

flatten_test_desc({list,Trace}) ->
    flatten_test_desc({inorder,Trace});

% flatten_test_desc(dummy) ->
%     [dummy];

% flatten_test_desc({_,_}) ->
%     [[dummy]];

flatten_test_desc(L) when is_list(L) ->
    lists:map(fun flatten_test_desc/1,L).
    
%% Accumulate the information from multiple items
%% into a single titem, repeatedly through the list.

%% The patterns in the function clause heads show
%% what is accumulated.

make_titems([{?tracing,line,[N]}|Items]) ->
    make_titems(N,[Item || Item<-Items, Item /= {?tracing,line,[N]} ]).

make_titems(N,
	    [{M,F,Args},
	     {{M,F,Arity},Res},
	     {?tracing,Pol,[Pat]}|
	     Rest]) ->
    [#titem{id = N,
	    mod = M,   
	    func = F,   
	    arity = Arity, 
	    args = Args,   
	    pn = case Pol of
		     test_positive -> pos;     
		     test_negative -> neg;
		     _ -> throw(polarity)
		 end,     
	    pat = Pat,   
	    res = Res}|
    make_titems(N,Rest)];

make_titems(N,
	    [{M,F,Args},
	     {{M,F,Arity},Res}|
	     Rest]) ->
    [#titem{id = N,
	    mod = M,   
	    func = F,   
	    arity = Arity, 
	    args = Args,   
	    res = Res}|
    make_titems(N,Rest)];

make_titems(N,
	    [{M,F,Args},
	     {?tracing,Pol,[Pat]}|
	     Rest]) ->
    [#titem{id = N,
	    mod = M,   
	    func = F,   
	    args = Args,   
	    pn = case Pol of
		     test_positive -> pos;     
		     test_negative -> neg;
		     _ -> throw(polarity)
		 end,     
	    pat = Pat}|
    make_titems(N,Rest)];

make_titems(_N,Xs) -> Xs.

%% Split a list at the first occurrence of
%%  {eunit_tracing,test_negative,_}
%% or when the list exhausted. In the first case
%% tag with neg, otherwise with pos.
    
posneg(Trace) ->
  case lists:splitwith(fun(Titem) ->
			  case Titem#titem.pn of
			      neg -> false;
			      _   -> true
			  end
		  end,
		  Trace) of
      {Xs,[Y|Ys]} -> {Xs++[Y],Ys,neg};
      {Xs,Zs}     -> {Xs,Zs,pos}
  end.

%% Push an item onto whichever of the positive or
%% negative lists it belongs.

push_posneg(Item, {P, N}) ->
    case posneg(Item) of
	{T, _, pos} -> { [T| P], N};
	{T, _, neg} -> {P, [T| N]}
    end.


%%%-------------------------------------------------------------------
%%
%% Auxiliary functions.
%%
%%%-------------------------------------------------------------------

%% Takes the atom to its "stem", i.e. takes the atom
%% blah_tests to blah, any other atom to itself.

stem(Atom) ->
    Rev = atom_to_list(Atom),
    Len = length(Rev),
    case lists:split(Len-6,Rev) of
	{Ans,"_tests"} ->
	     list_to_atom(Ans);
	_ -> Atom
    end.

%% Ensuring the correct nesting of (lists of)* items.

join(Xs) -> lists:concat(lists:concat(Xs)).


%%%-------------------------------------------------------------------
%%
%% Consistency checking for pairs of trace sets.
%%
%%%-------------------------------------------------------------------

%% Check for consistency of sets of positive and
%% negative traces. 

%% Look for lists in Neg which are initial segments of
%% a list in Pos (since positive traces are closed under
%% initial segment).

consistent({Pos,Neg}) ->
    consis_check({Pos,Neg}) == [].

consis_check({Pos,Neg}) ->
    [ {Ps,Ns} || Ps <- Pos,
		 Ns <- Neg, is_initial(Ps,Ns) ].

%% is_initial(Xs,Ys) is true iff Ys is an
%% initial segment of Xs.

is_initial(_,[]) ->
    true;
is_initial([I|Ps],[J|Ns])
  when I==J ->
    is_initial(Ps,Ns);
is_initial(_,_) ->
    false.


%%%-------------------------------------------------------------------
%%
%% Tests.
%%
%%%-------------------------------------------------------------------
	
test1() ->
    start(tradepost_tests, api()).

test2() ->
    start(frequency_tests).

api() ->
    [start_link,introspection_statename,introspection_loopdata,stop,seller_identify,seller_insertitem,withdraw_item].
     

