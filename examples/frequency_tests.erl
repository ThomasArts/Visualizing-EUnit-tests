%% Code testing frequency.erl which is itself from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_tests).
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

stopFirst_test_() ->
     ?_assertError(badarg,stop()).   % stop before start causes failure
    	     
startTwice_test_() ->
    {setup,
     fun ()  -> start([]) end,        % normal startup here
     fun (_) -> stop() end,         % stop the system after the test
     ?_assertError(badarg,start([]))  % a second start causes failure
    }.

%% stopTwice_test_() ->
%%     start(),stop(),?_assertError(badarg,stop())   % a second stop causes failure

%% fourNegative_test_() ->
%%     {setup,
%%      fun ()  -> start(),stop(),start() end, % start;stop:start
%%      fun (_) -> stop() end,         % stop the system
%%      ?_assertError(badarg,start())  % can't start once started
%%     }.


%% start(), stop() and allocate(): series of allocates
%% note: not testing for the particular frequency returned, but rather
%% that a successful allocation has taken place.
    
%% allocateFirst_test_() ->
%%     {setup,
%%      fun ()  -> ok end,             % null startup here
%%      fun (_) -> ok end,             % no cleanup to do
%%      ?_assertError(badarg,allocate())  % allocate before start causes failure
%%     }.

%% allocate1_test_() ->
%%     {setup,
%%      fun ()  -> start() end,        % normal startup 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertMatch({ok,_},allocate())  % one allocate is OK
%%     }.
	     
%% allocate2_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate() end, % allocate one 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertMatch({ok,_},allocate())  % two allocates OK
%%     }.
	     
%% allocate3_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate() end, % allocate two 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertMatch({ok,_},allocate())  % three allocates OK
%%     }.
	       	     
%% allocate4_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate() end, % allocate three 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertMatch({ok,_},allocate())  % four allocates OK
%%     }.  
	      	     
%% allocate5_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate() end, % allocate four 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertMatch({ok,_},allocate())  % five allocates OK
%%     }.

%% allocate6_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate(),allocate() end, % allocate five 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertMatch({ok,_},allocate())  % six allocates OK
%%     }.

%% allocate7_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate(),allocate(),allocate() end, % allocate six 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertMatch({error,no_frequency},allocate())  % seven allocates not OK
%%     }.


%% %% start(), stop() and allocate(): series of allocates then start    
	     
%% allocate1start_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate() end, % allocate one 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertError(badarg,start())  % second start fails
%%     }.
	     
%% allocate2start_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate() end, % allocate two 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertError(badarg,start())  % second start fails
%%     }.

%% allocate3start_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate() end, % allocate three 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertError(badarg,start())  % second start fails
%%     }.  	      	   	       	     
	      	     
%% allocate4start_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate() end, % allocate four 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertError(badarg,start())  % second start fails
%%     }.

%% allocate5start_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate(),allocate() end, % allocate five 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertError(badarg,start())  % second start fails
%%     }.

%% allocate6start_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate(),allocate(),allocate() end, % allocate six 
%%      fun (_) -> stop() end,         % cleanup
%%      ?_assertError(badarg,start())  % second start fails
%%     }.


%% %% start(), stop() and allocate(): series of allocates then stop    
	     
%% allocate1stop_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate() end, % allocate one 
%%      fun (_) -> ok end,             % no cleanup to do
%%      ?_assertMatch(ok,stop())       % stop succeeds
%%     }.
	     
%% allocate2stop_test_() ->
%%     {setup,
%%      fun ()  -> start() end, % allocate two 
%%      fun (_) -> ok end,             % no cleanup to do
%%       allocate(), allocate(), ?_assertMatch(ok,stop())       % stop succeeds
%%     }.

%% allocate3stop_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate() end, % allocate three 
%%      fun (_) -> ok end,             % no cleanup to do
%%      ?_assertMatch(ok,stop())       % stop succeeds
%%     }.  	      	   	       	     
	      	     
%% allocate4stop_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate() end, % allocate four 
%%      fun (_) -> ok end,             % no cleanup to do
%%      ?_assertMatch(ok,stop())       % stop succeeds
%%     }.

%% allocate5stop_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate(),allocate() end, % allocate five 
%%      fun (_) -> ok end,             % no cleanup to do
%%      ?_assertMatch(ok,stop())       % stop succeeds
%%     }.

%% allocate6stop_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),allocate(),allocate(),allocate() end, % allocate six 
%%      fun (_) -> ok end,             % no cleanup to do
%%      ?_assertMatch(ok,stop())       % stop succeeds
%%     }.

%% %% Other possibilities for start(), stop() and allocate()
%% %%
%% %%  - interleaving the other tests for start() and stop()
%% %%    with allocate(): should expect same results, except that
%% %%    can't put any allocate() calls between stop() and start() 

%% %% Worth noting that these three operations are without parameters, 
%% %% and so relatively easy to test: no data dependencies to worry about.


%% %% Allocate and deallocate: 
%% %% deallocation always works

%% allocDealloc_3_1_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),
%%                 deallocate(10) end,    % allocate three; dellocate first
%%      fun (_) -> stop() end,            % cleanup
%%      ?_assertMatch({ok,10},allocate()) % LIFO behaviour
%%     }.    

%% allocDealloc_3_2_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),
%%                 deallocate(11) end,    % allocate three; dellocate second
%%      fun (_) -> stop() end,            % cleanup
%%      ?_assertMatch({ok,11},allocate()) % LIFO behaviour
%%     }.    

%% allocDealloc_3_3_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),
%%                 deallocate(12) end,    % allocate three; dellocate third
%%      fun (_) -> stop() end,            % cleanup
%%      ?_assertMatch({ok,12},allocate()) % LIFO behaviour
%%     }.    

%% allocDealloc_3_4_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),
%%                 deallocate(13) end,    % allocate three; dellocate another
%%      fun (_) -> stop() end,            % cleanup
%%      ?_assertMatch({ok,13},allocate()) % LIFO behaviour
%%     }.    

%% allocDealloc_3_5_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),
%%                 deallocate(14) end,    % allocate three; dellocate another
%%      fun (_) -> stop() end,            % cleanup
%%      ?_assertMatch({ok,14},allocate()) % LIFO behaviour
%%     }.    

%% allocDealloc_3_6_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(),
%%                 deallocate(15) end,    % allocate three; dellocate another
%%      fun (_) -> stop() end,            % cleanup
%%      ?_assertMatch({ok,15},allocate()) % LIFO behaviour
%%     }.    


%% %% Allocate and deallocate: 
%% %% deallocation of an allocated value subsequently allows one 
%% %% more allocation than deallocation of something which is not 
%% %% already allocated.

%% allocDeallocAlloc_3_1_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(), % allocate three
%%                 deallocate(10),                           % dellocate first
%% 		allocate(),allocate(),allocate() end,     % allocate three
%%      fun (_) -> stop() end,                               % cleanup
%%      ?_assertMatch({ok,_},allocate())                     % one left
%%     }.    

%% allocDeallocAllocNeg_3_1_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(), % allocate three
%%                 deallocate(10),                           % dellocate first
%% 		allocate(),allocate(),allocate(),
%%                 allocate() end,                           % allocate four
%%      fun (_) -> stop() end,                               % cleanup
%%      ?_assertMatch({error,_},allocate())                     % none left
%%     }.    

%% allocDeallocAlloc_3_2_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(), % allocate three
%%                 deallocate(11),                           % dellocate second
%% 		allocate(),allocate(),allocate() end,     % allocate three
%%      fun (_) -> stop() end,                               % cleanup
%%      ?_assertMatch({ok,_},allocate())                     % one left
%%     }.    

%% allocDeallocAllocNeg_3_2_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(), % allocate three
%%                 deallocate(11),                           % dellocate first
%% 		allocate(),allocate(),allocate(),
%%                 allocate() end,                           % allocate four
%%      fun (_) -> stop() end,                               % cleanup
%%      ?_assertMatch({error,_},allocate())                  % none left
%%     }.    

%% allocDeallocAlloc_3_3_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(), % allocate three
%%                 deallocate(12),                           % dellocate third
%% 		allocate(),allocate(),allocate() end,     % allocate three
%%      fun (_) -> stop() end,                               % cleanup
%%      ?_assertMatch({ok,_},allocate())                     % one left
%%     }.   
 
%% allocDeallocAllocNeg_3_3_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(), % allocate three
%%                 deallocate(12),                           % dellocate first
%% 		allocate(),allocate(),allocate(),
%%                 allocate() end,                           % allocate four
%%      fun (_) -> stop() end,                               % cleanup
%%      ?_assertMatch({error,_},allocate())                  % none left
%%     }.    

%% allocDeallocAlloc_3_4_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(), % allocate three
%%                 deallocate(13),                           % dellocate another
%% 		allocate(),allocate() end,                % allocate two
%%      fun (_) -> stop() end,                               % cleanup
%%      ?_assertMatch({ok,_},allocate())                     % one left
%%     }.    
    
%% allocDeallocAllocNeg_3_4_test_() ->
%%     {setup,
%%      fun ()  -> start(),allocate(),allocate(),allocate(), % allocate three
%%                 deallocate(13),                           % dellocate another
%% 		allocate(),allocate(),allocate() end,     % allocate three
%%      fun (_) -> stop() end,                               % cleanup
%%      ?_assertMatch({error,_},allocate())                  % none left
%%     }.    
