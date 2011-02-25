%% Code testing frequency.erl which is itself from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_tests).
-include("../include/eunit_to_fsm.hrl").  %% later include_lib, only has effect if EUNIT_HRL is defined
-include_lib("eunit/include/eunit.hrl").
-import(frequency,[start/1, stop/0, allocate/0, deallocate/1,init/0]).


%% start() and stop()


%% The following 3 tests result in 3 sequences that result in the complete spec for stop and restart
%% + start stop start (stop)     stop could be in cleanup, cleanup may be discarded in translation
%% - stop
%% - start start
%%
%% Receipe: we first wrote those 3 with startstop_test only start and stop, when visualizing, 
%% you realize that you have to state that indeed one can start again.

startstop_test() ->
     ?_assertMatch(true,start([])),
     ?_assertMatch(ok,stop()),
     ?_assertMatch(true,start([])),
     ?_assertMatch(ok,stop()).

stopFirst_test() ->
     ?_assertError(badarg,stop()).   % stop before start causes failure
    	     
startTwice_test_() ->
    {setup,
     fun ()  -> start([]) end,        % normal startup here
     fun (_) -> stop() end,         % stop the system after the test
     fun () -> ?_assertError(badarg,start([])) end  % a second start causes failure
    }.

stopTwice_test() ->
    start([]),stop(),?_assertError(badarg,stop()).   % a second stop causes failure

%% start(), stop() and allocate(): series of allocates
%% note: not testing for the particular frequency returned, but rather
%% that a successful allocation has taken place.
    
allocateFirst_test_() ->
  {setup,
   fun ()  -> ok end,             % null startup here
   fun (_) -> ok end,             % no cleanup to do
   fun () -> ?_assertError(badarg,allocate()) end  % allocate before start causes failure
  }.

allocate1_test_() ->
    {setup,
     fun ()  -> start([1]) end,        % normal startup 
     fun (_) -> stop() end,         % cleanup
     fun () -> ?_assertMatch({ok,_},allocate()) end  % one allocate is OK
    }.
	     
allocate2_test_() ->
    {setup,
     fun ()  -> start([1]) end,        % normal startup
     fun (_) -> stop() end,         % cleanup
     fun () -> allocate(), ?_assertMatch({ok,_},allocate()) end  % two allocates OK
    }.
	     
allocate3_test_() ->
    {setup,
     fun ()  -> start([]),allocate(),allocate() end, % normal startup 
     fun (_) -> stop() end,         % cleanup
     fun () -> allocate(), ?_assertError(_,start()) end  % second start fails
    }.
	       	     
allocate_dealloc_test_() ->
 {setup,
  fun ()  -> start([1]) end, 
  fun (_) -> stop() end, % cleanup
  fun () ->
    ?_assertMatch({ok,V},allocate()),
    ?_assertMatch(_,deallocate(V)),
    ?_assertMatch({ok,_},allocate())
  end  
 }.

