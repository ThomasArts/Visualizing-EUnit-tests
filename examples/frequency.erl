%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-include_lib("eunit/include/eunit.hrl").
-export([start/1, stop/0, allocate/0, deallocate/1, skip/0, use_skip/0]).
-export([init/1]).

%% These are the start functions used to create and
%% initialize the server.

start(Freqs) ->
  register(frequency, spawn(frequency, init, [Freqs])).

init(Freqs) ->
  Frequencies = {Freqs, []},
  loop(Frequencies).

% Hard Coded
% get_frequencies() -> [10,11,12,13,14,15].

%%  The client Functions

stop()           -> call(stop).
allocate()       -> {ok,_} = call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.

call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      reply(Pid, ok),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      reply(Pid, ok)
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

%% Included for purposes of testing
%% the details of tracing: do calls to
%% functions in frequency from other
%% functions in frequency get traced too?

%% Answer: only when those calls are fully qualified.

skip() ->
    case call(allocate) of
	{ok,N} ->
	    frequency:deallocate(N),
	    ok;
	_ -> ok
    end.

use_skip() ->
    frequency:allocate(),frequency:skip(),frequency:deallocate(1),frequency:skip().
