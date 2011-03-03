%% Code testing frequency.erl which is itself from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_tests).

%-include("../include/eunit_to_fsm.hrl").
%% later include_lib, only has effect if EUNIT_HRL is defined

-include_lib("eunit/include/eunit.hrl").
-import(frequency,[start/1, stop/0, allocate/0, deallocate/1]).

%% These are wrappers for assertXXX and _assertXXX fromEUnit
%% Replace the EUnit calls with these.

%% TODO modify EUnit so that don't need to change macro calls.
%% or find a clever way of achieving the same effect without
%% modifying EUnit. Note that since these are macros, need to do
%% at/before macro processing stage.

-define(assertErrorTrace(X,Y),tracing:test_negative(?assertError(X,Y))).
-define(_assertErrorTrace(X,Y),tracing:negative_wrap(?_assertError(X,Y))).


%%
%% The tests themselves
%%

%% A single positive test.

startstop_test() ->
    tracing:test_wrap(fun () ->
                        ?assertMatch(true,start([])),
                        ?assertMatch(ok,stop()),
                        ?assertMatch(true,start([])),
                        ?assertMatch(ok,stop())
	              end).

%% A group of positive tests.

startstop_test_() ->
    tracing:test__wrap([ ?_assertMatch(true,start([])),
		               ?_assertMatch(ok,stop()),
		               ?_assertMatch(true,start([])),
		               ?_assertMatch(ok,stop())]).

%% A single negative test.

stopFirst_test() ->
    tracing:test_wrap(fun () ->
		              ?assertErrorTrace(badarg,stop())
	              end).  

%% A fixture that also contains a negative test.

startTwice_test_() ->
    tracing:test__wrap([{setup,
		         fun () -> start([]) end,
		         fun (_) -> stop() end,
		         ?_assertErrorTrace(badarg,start([]))
		        }]).

% stopTwice_test_() ->
%   start([]),stop(),?_assertError(badarg,stop()).   % a second stop causes failure


% %% start(), stop() and allocate(): series of allocates
% %% note: not testing for the particular frequency returned, but rather
% %% that a successful allocation has taken place.
    
% allocateFirst_test_() ->
%   {setup,
%    fun ()  -> ok end,             % null startup here
%    fun (_) -> ok end,             % no cleanup to do
%    ?_assertError(badarg,allocate())  % allocate before start causes failure
%   }.

% allocate1_test_() ->
%   {setup,
%    fun ()  -> start([1]) end,        % normal startup 
%    fun (_) -> stop() end,         % cleanup
%    ?_assertMatch({ok,_},allocate())  % one allocate is OK
%   }.
	     
% allocate2_test_() ->
%   {setup,
%    fun ()  -> start([1]) end, % allocate one 
%    fun (_) -> stop() end, % cleanup
%    begin
%      allocate(),
%      ?_assertError(_,allocate())
%    end   % two allocates OK
%   }.

% allocate3_test_() ->
%   {setup,
%    fun ()  -> start([1]) end, % allocate one 
%    fun (_) -> stop() end, % cleanup
%    begin
%      allocate(),
%      ?_assertError(_,start([1]))
%    end   % two allocates OK
%   }.

% allocate4_test_() ->
%   {setup,
%    fun ()  -> start([1]) end, % allocate one 
%    fun (_) -> stop() end, % cleanup
%    begin
%      allocate(),
%      ?_assertMatch(_,start([]))
%    end   % two allocates OK
%   }.


% allocate_dealloc_test_() ->
%   {setup,
%    fun ()  -> start([1]) end, 
%    fun (_) -> stop() end, % cleanup
%    begin
%      ?_assertMatch({ok,V},allocate()),
%      ?_assertMatch(_,deallocate(V)),
%      ?_assertMatch({ok,_},allocate())
%    end  
%   }.
