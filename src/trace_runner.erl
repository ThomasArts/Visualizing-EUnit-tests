-module(trace_runner).

% Runs the functions from Module:test() with
% tracing as provided by eunit_tracing.erl

-export([start/2,tester/2,exclude/1]).

-include("../include/tracing.hrl").

% Top-level function.
% Returns the traces as a list of (lists of) lists.

start(Module,Hide) ->
    eunit_tracing:t(),
    spawn(?MODULE,tester,[Module,self()]),
    Msgs = loop([]),
    Trace = process(Msgs,Hide),
    split_traces(Trace).

% Testing process: runs Module:test()
% and then tells Pid it has finished.

tester(Module,Pid) ->
    Module:test(),
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
    {Trace,[{?tracing,test_end,[]}|Msgs]}
	= lists:splitwith(fun (Msg) -> Msg /= {?tracing,test_end,[]} end,Rest),
    [Trace|split_traces(Msgs)];

split_traces([{?tracing,test_group_start,[]}|Rest]) ->
    {Trace,[{?tracing,test_group_end,[]}|Msgs]}
	= lists:splitwith(fun (Msg) -> Msg /= {?tracing,test_group_end,[]} end,Rest),
    [split_traces(Trace)|split_traces(Msgs)];

split_traces(Msgs) ->
    {Trace,Rest}
	= lists:splitwith(fun not_end/1,Msgs),
     [Trace|split_traces(Rest)].

% Aux function: used in splitting checks for "start" messages.

not_end(Msg) ->
    Msg /= {?tracing,test_group_start,[]} andalso  Msg /= {?tracing,test_start,[]}
    
.


