-module(trace_runner).

% Runs the functions from Module:test() with
% tracing as provided by eunit_tracing.erl

-export([start/1,start/2,tester/2,consis_check/1,consistent/1,parse/1]).

-export([posneg/1]).

-compile(export_all).


-include("../include/tracing.hrl").

% Top-level function.
% Returns the traces as a list of (lists of) lists.

start(Module) ->
    start(Module,fr_abstraction()).

start(Module, Abstraction) ->
    eunit_tracing:t(),
    spawn(?MODULE, tester, [Module, self()]),
    Msgs = loop([]),
    Calls = [Call || {_, _, _, Call} <- Msgs],
    io:format("Printing calls~n",[]),
    io:format("~p~n",[Calls]),
    CallsF = process(Calls,Abstraction),
    io:format("Printing filtered calls~n",[]),
    io:format("~p~n",[CallsF]),
    Structs = parse(CallsF),
    io:format("Printing parsed structure~n",[]),
    io:format("~p~n",[Structs]),
    Traces = flatten_struct(Structs),
    io:format("Printing traces~n",[]),
    io:format("~p~n",[Traces]),
    AssertedTraces = [ assertions(T) || T<-Traces],
    io:format("Printing asserted traces~n",[]),
    io:format("~p~n",[AssertedTraces]),
    %% Here we should now get the assertions of also positive tests into the trace
    %% After that, we can make 4 tuples instead of 3 tuples of each event.
    lists:foldl(fun (Trace, {P, N}) ->
     			case posneg(Trace) of
			     {T, []} -> { [T| P], N};
			     {T, _} -> {P, [T| N]}
     			end
     		 end, {[], []}, Traces).


% Testing process: runs Module:test()
% and then tells Pid it has finished.

% The one second pause is to ensure that
% all trace messages are collected; without this
% some carry over to the next call ... 

tester(Module,Pid) ->
    Module:test(),
    receive
    after 1000 ->
	    ok
    end,
    Pid ! finished.

% Loop to collect messages in a list
% until the finished message is received.

loop(Msgs) ->
    receive
	finished ->
	      lists:reverse(Msgs);
	Msg ->
	    loop([Msg|Msgs])
    end.

% process messages
% Hide messages by the Hide function and abstraction function.
% Calls combined with their abstractions.

process(Msgs,Hide) ->
  lists:reverse(lists:foldl(fun(Call,Acc) ->
                  case catch abstraction(Hide,Call) of
                    {'EXIT',_} -> Acc;
                    _Other -> [ Call | Acc ]
                  end
              end,[],Msgs)).

% Combine the effect of the hiding function Hide with the
% hiding of trace info from module_info and test functions
% in all modules.

abstraction(Hide, Call) ->
    fun ({M, F, A}) when F =/= module_info orelse F =/= test -> Hide({M, F, A})
    end(Call).

% Computes a particular hiding function: hide the elements
% on which the function is not defined. In the defalut case
% this is the function frequency:init. 

fr_abstraction() ->
  fun({M,F,A}) when {M,F}=/={frequency,init} -> {M,F,A} end.
    
% Parses a list of messages into a nested format reflecting
% the structures in the test descriptions.
% Assumes that the list is well formed - start/end messages
% are properly matched, and so parse by deterministic recursive
% descent.

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


% Flatten structure - coming from parse - into a single list of traces.
% Result is a list of traces (i.e. a list of lists).

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

flatten_test_desc({list,Trace}) ->
    flatten_test_desc({inorder,Trace});

flatten_test_desc(dummy) ->
    [dummy];

flatten_test_desc({_,_}) ->
    [[dummy]];

flatten_test_desc(L) when is_list(L) ->
    L.


% Ensuring the correct nesting of (lists of)* ...


join(Xs) -> lists:concat(lists:concat(Xs)).    

% Check for consistency
% Returns all inconsistent pairs, if any.

% Looking for lists in Neg which are initial segments of
% a list in Pos (since positive traces are closed under
% initial segment).

consistent({Pos,Neg}) ->
    consis_check({Pos,Neg}) == [].

consis_check({Pos,Neg}) ->
    [ {Ps,Ns} || Ps <- Pos,
		 Ns <- Neg, is_initial(Ps,Ns) ].

% is_initial(Xs,Ys) is true iff Ys is an
% initial segment of Xs.

is_initial(_,[]) ->
    true;
is_initial([I|Ps],[J|Ns])
  when I==J ->
    is_initial(Ps,Ns);
is_initial(_,_) ->
    false.

% Split a list before the first occurrence of
%  {eunit_tracing,test_negative,_}
% Second element of the pair will be [] iff that
% tuple doesn't occur, i.e. trace is positive.
    
posneg(Trace) ->
  lists:splitwith(fun({eunit_tracing,test_negative,_}) ->
                       false;
                      (_) ->
                       true
                   end,Trace).
    

assertions(Trace) ->
  lists:foldr(fun({eunit_tracing,test_negative,[Assert]},NT) ->
                  [{eunit_tracing,assertion,[Assert]},{eunit_tracing,test_negative,[]}|NT];
                 ({eunit_tracing,test_negative,[Assert,R]},NT) ->
                  [{eunit_tracing,assertion,[Assert]},{eunit_tracing,test_negative,[R]}|NT];
                 (T,NT) ->
                  [T|NT]
              end,[],Trace).
    
