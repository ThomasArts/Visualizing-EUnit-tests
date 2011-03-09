-module(trace_runner).

% Runs the functions from Module:test() with
% tracing as provided by eunit_tracing.erl

-export([start/1,start/2,tester/2,consis_check/1,consistent/1]).

-export([posneg/1]).

-include("../include/tracing.hrl").

% Top-level function.
% Returns the traces as a list of (lists of) lists.

start(Module) ->
    start(Module,fr_abstraction()).

start(Module, Abstraction) ->
    eunit_tracing:t(),
    spawn(?MODULE, tester, [Module, self()]),
    Msgs = loop([]),
    Traces = split_traces([Call || {_, _, _, Call} <- Msgs]),
    %% flatten to one list with traces (no nesting)
    PosNeg =
	lists:foldl(fun (Trace, {P, N}) ->
			    case posneg(Trace) of
			      {T, []} -> {[process(T, Abstraction)| P], N};
			      {T, _} -> {P, [process(T, Abstraction)| N]}
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
    Pid!finished.

% Loop to collect messages in a list
% until the finish message is received.

loop(Msgs) ->
    receive
	finished ->
	      lists:reverse(Msgs);
	Msg ->
	    loop([Msg|Msgs])
    end.

% process messages
%  - remove the outermost tuple
%  - remove calls in the Hide list.

process(Msgs,Hide) ->
  lists:foldl(fun(Call,Acc) ->
                  case catch abstraction(Hide,Call) of
                    {'EXIT',_} -> Acc;
                    Other -> [ {Other,Call} | Acc ]
                  end
              end,[],Msgs).

%% How do we ensure that M==Mod
abstraction(Hide, Call) ->
    fun ({M, F, A}) when F =/= module_info orelse F =/= test -> Hide({M, F, A})
    end(Call).

% Computes a particular Hide list.
% Always want to exclude the latter two elements.

fr_abstraction() ->
  fun({M,F,_}) when {M,F}=/={frequency,init} -> {M,F} end.
    
% Splits a list of messages into traces.
% Assumes that the list is well formed - start/end messages
% are properly matched.

split_traces([])-> 
    [];

split_traces([{?tracing,test_start,[]}|Rest]) ->
    case lists:splitwith(fun (Msg) -> Msg /= {?tracing,test_end,[]} end,Rest) of 
	{Trace,[{?tracing,test_end,[]}|Msgs]} -> 
	    [Trace|split_traces(Msgs)];
	{_,_} ->
	    erlang:error("no matching test_end")
    end;

split_traces([{?tracing,test_group_start,[]}|Rest]) ->
    case  lists:splitwith(fun (Msg) -> Msg /= {?tracing,test_group_end,[]} end,Rest) of
	{Trace,[{?tracing,test_group_end,[]}|Msgs]} -> 
	    [split_traces(Trace)|split_traces(Msgs)];
	{_,_} ->
	    erlang:error("no matching test_group_end")
    end;

split_traces(Msgs) ->
    {Trace,Rest}
	= lists:splitwith(fun not_end/1,Msgs),
     [Trace|split_traces(Rest)].

% Aux function: used in splitting checks for "start" messages.

not_end(Msg) ->
    Msg /= {?tracing,test_group_start,[]} andalso  Msg /= {?tracing,test_start,[]}.

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

is_initial(_,[]) ->
    true;
is_initial([I|Ps],[J|Ns])
  when I==J ->
    is_initial(Ps,Ns);
is_initial(_,_) ->
    false.
    
posneg(Trace) ->
  lists:splitwith(fun({eunit_tracing,test_negative,_}) ->
                       false;
                      (_) ->
                       true
                   end,Trace).
    

