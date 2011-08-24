-module(tradepost_tests).
-include_lib("eunit/include/eunit.hrl").

% This is the main point of "entry" for my EUnit testing.
% A generator which forces setup and cleanup for each test in the testset

main_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     % Note that this must be a List of TestSet or Instantiator
     % (I have instantiators)
     [
      % First Iteration
      fun started_properly/1,
      % Second Iteration
      fun identify_seller/1,
      fun insert_item/1,
      fun withdraw_item/1
     ]}.



% Setup and Cleanup
setup()      -> {ok,Pid} = tradepost:start_link(), Pid.
cleanup(Pid) -> tradepost:stop(Pid).

% Pure tests below
% ------------------------------------------------------------------------------
% Let's start simple, I want it to start and check that it is okay.
% I will use the introspective function for this


started_properly(Pid) ->
    fun() ->
            ?assertEqual(pending,tradepost:introspection_statename(Pid)),
            ?assertEqual([undefined,undefined,undefined,undefined,undefined],
                         tradepost:introspection_loopdata(Pid))
    end.

% Now, we are adding the Seller API tests
identify_seller(Pid) ->
    fun() ->
            % From Pending, identify seller, then state should be pending
            % loopdata should now contain seller_password
            ?assertEqual(pending,tradepost:introspection_statename(Pid)),
            ?assertEqual(ok,tradepost:seller_identify(Pid,seller_password)),
            ?assertEqual(pending,tradepost:introspection_statename(Pid)),
            ?assertEqual([undefined,undefined,seller_password,undefined,
                       undefined],tradepost:introspection_loopdata(Pid))
    end.

insert_item(Pid) ->
    fun() ->
            % From pending and identified seller, insert item
            % state should now be item_received, loopdata should now contain itm
            tradepost:introspection_statename(Pid),
            tradepost:seller_identify(Pid,seller_password),
            ?assertEqual(ok,tradepost:seller_insertitem(Pid,playstation,
                                                  seller_password)),
            ?assertEqual(item_received,tradepost:introspection_statename(Pid)),
            ?assertEqual([playstation,undefined,seller_password,undefined,
                       undefined],tradepost:introspection_loopdata(Pid))
    end.

withdraw_item(Pid) ->
    fun() ->
            % identified seller and inserted item, withdraw item
            % state should now be pending, loopdata should now contain only password
            tradepost:seller_identify(Pid,seller_password),
            tradepost:seller_insertitem(Pid,playstation,seller_password),
            ?assertEqual(ok,tradepost:withdraw_item(Pid,seller_password)),
            ?assertEqual(pending,tradepost:introspection_statename(Pid)),
            ?assertEqual([undefined,undefined,seller_password,undefined,
                       undefined],tradepost:introspection_loopdata(Pid))
    end.
