%% Code testing frequency.erl which is itself from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_tests_static).
-include_lib("eunit/include/eunit.hrl").
-import(frequency,[start/1, stop/0, allocate/0, deallocate/1]).


%% The following 3 tests result in 3 sequences that result in the complete spec for stop and restart
%% + start stop start (stop)     stop could be in cleanup, cleanup may be discarded in translation
%% - stop
%% - start start
%%
%% Receipe: we first wrote those 3 with startstop_test only start and stop, when visualizing, 
%% you realize that you have to state that indeed one can start again.

startstop_test() ->
    start([]),
    ?assertMatch(ok,stop()), 
    start([1]),
    ?assertMatch(ok,stop()).

stop_without_start_test() ->
    ?assertException(_,_,stop()). 

allocate_without_start_test() ->
    ?assertException(_,_,allocate()). 

deallocate_without_start_test() ->
    ?assertException(_,_,deallocate(1)). 

running_server_test_() ->
    {setup,
     fun() -> start([]) end,
     fun(_) -> stop() end,
     fun() -> start_again() end}.

running_server2_test_() ->
    {foreach,
     fun() -> start([1]) end,
     fun(_) -> stop() end,
     [fun() ->  
	      ?assertMatch({ok,1} ,allocate()),
	      ?assertMatch(ok,deallocate(1)),
	      ?assertMatch({ok,1},allocate())
      end,
     fun() -> 
	     ?assertMatch({ok,1} ,allocate()),
	     ?assertMatch({error,no_frequency} ,allocate())
     end,
      fun() -> 
	     ?assertMatch({ok,1} ,allocate()),
	     ?assertMatch(ok,stop()),
	     start([1])
      end
     ]}.

start_again() ->
    ?assertException(_,_,start([])).


