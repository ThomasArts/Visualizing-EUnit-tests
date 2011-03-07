-module(trace_runner).

% Runs the functions from Module:test() with
% tracing as provided by eunit_tracing.erl

-export([start/1,start/2,tester/2,exclude/1,consis_check/1,consistent/1]).

-include("../include/tracing.hrl").

% Top-level function.
% Returns the traces as a list of (lists of) lists.

start(Module) ->
    start(Module,exclude(Module)).

start(Module,Hide) ->
    eunit_tracing:t(),
    spawn(?MODULE,tester,[Module,self()]),
    Msgs = loop([]),
    Trace = process(Msgs,Hide),
    split_traces(Trace).

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
    [ Call || {_,_,_,Call} <- Msgs, not(lists:member(mf_name(Call),Hide)) ].

mf_name({M,F,_}) ->
    {M,F}.

% Computes a particular Hide list.
% Always want to exclude the latter two elements.
    
exclude(Module) ->
     [{frequency,init},{Module,module_info},{Module,test}].

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
    
	       
    

