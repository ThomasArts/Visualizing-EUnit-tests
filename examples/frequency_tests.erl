%% Code testing frequency.erl which is itself from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_tests).
-include_lib("eunit/include/eunit.hrl").
-import(frequency,[start/1, stop/0, allocate/0, deallocate/1, skip/0, use_skip/0]).


%% Four positive tests.

startstop_test_() ->
    {inorder,
     [ ?_assertMatch(true,start([])),
       ?_assertMatch(ok,stop()),
       ?_assertMatch(true,start([1])),
       ?_assertMatch(ok,stop())]}.


% A single negative test.

stopFirst_test() ->
     ?assertError(badarg,stop()).

% A fixture that also contains a negative test.

startTwice_test_() ->
  [{setup,
    fun ()  -> start([1]) end,       
    fun (_) -> stop() end,         
      ?_assertError(badarg,start([1,2]))  
   }].

% A single allocate

allocate_test_() ->
  [{setup,
    fun ()  -> start([1]) end,       
    fun (_) -> stop() end,
    ?_assertMatch({ok,1},allocate())
   }].

     % allocate2_test_() ->
     %      {setup,
     %       fun () -> ok end,
     %       fun(_) -> stop() end,
     %       fun () ->  ?assertMatch(true,frequency:start([1])),
     % 		 ?assertMatch({ok,1},frequency:allocate()),
     % 		 ?assertError({badmatch,{error,no_frequency}},frequency:allocate())
     %       end}.

   % allocate3_test_() ->
   %      {setup,
   %       fun () -> ok end,
   %       fun(_) -> stop() end,
   %       fun () ->  ?assertMatch(true,frequency:start([])),
   %                  ?assertError({badmatch,{error,no_frequency}},frequency:allocate())
   %       end}.


 % xxx_test_() ->
 %     {setup,
 %      fun () -> ok end,
 %      fun(_) -> stop() end,
 %      fun () ->
 % 	     yyy
 %      end}.

% modNew_test_() ->
%     {setup,
%      fun () -> ok end,
%      fun(_) -> stop() end,
%      fun () ->
% 	     ?assertMatch(true,start([1])),
% 	     ?assertMatch({ok,1},allocate()),
% 	     ?assertError({badmatch,{error,no_frequency}},allocate())
%      end}.

% modNew2_test_() ->
%     {setup,
%      fun () -> ok end,
%      fun(_) -> stop() end,
%      fun () ->
% 	     ?assertMatch(true,start([])),
% 	     ?assertError({badmatch,{error,no_frequency}},allocate())
%      end}.

% modNew3_test_() ->
%     {setup,
%      fun () -> ok end,
%      fun(_) -> stop() end,
%      fun () ->
% 	     ?assertMatch(true,start([1])),
% 	     ?assertMatch(ok,stop()),
% 	     ?assertMatch(true,start([1, 2]))
%      end}.


% new_test() ->
%   ?assertMatch(true,frequency_eqc:start([1])),
%   ?assertMatch(ok,frequency_eqc:stop()),
%   ?assertMatch(true,frequency_eqc:start([1, 2])).


% new_test() ->
%   ?assertMatch(true,frequency_eqc:start([])),
%   ?assertError({badmatch,{error,no_frequency}},frequency_eqc:allocate()).
	      
  
 % new_test() ->
 %   ?assertMatch(true,start([1])),
 %   ?assertMatch({ok,1},allocate()),
 %   ?assertError({badmatch,{error,no_frequency}},allocate()).



