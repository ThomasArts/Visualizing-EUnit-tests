%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2010, Gianfranco
%%% Created : 10 Dec 2010 by Gianfranco <zenon@zen.local>
-module(wn_resource_layer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/worker_net.hrl").

local_resource_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Can register resources locally",fun register_locally/0}
     ]}.

  distr_resource_test_() ->
      {foreach,
       fun distr_setup/0,
       fun distr_cleanup/1,
       [fun register_distributed/1%,
       %  fun register_restart_register/1%,
       %  fun register_deregister/1
        ]
      }.

register_locally() ->
    ResourceA = #wn_resource{name = "macbook pro laptop",
                             type = [{'os-x',1},{bsd,1}],
                             resides = node()},
    ResourceB = #wn_resource{name = "erlang runtime system",
                             type = [{erlang,4}],
                             resides = node()},
    ok = wn_resource_layer:register(ResourceA),
    ok = wn_resource_layer:register(ResourceB),
    List = lists:sort(wn_resource_layer:list_resources()),
    ?assertMatch([ResourceB,ResourceA],List).

register_distributed([N1,N2]) ->
    {"Can Register Distributed",
     fun() ->
             rpc:call(N1,wn_resource_layer,start_link,[]),
             rpc:call(N2,wn_resource_layer,start_link,[]),
             ResourceA = #wn_resource{name = "erlang R14",
                                      type = [{erlang,infinity}],
                                      resides = N1},
             ResourceB = #wn_resource{name = "os-x macbook pro",
                                      type = [{'os-x',1}],
                                      resides = N2},
             ResourceC = #wn_resource{name = "g++",
                                      type = [{'g++',1}],
                                      resides = node()},
             ok = wn_resource_layer:register(ResourceA),
             ok = wn_resource_layer:register(ResourceB),
             ok = wn_resource_layer:register(ResourceC),
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(wn_resource_layer:list_resources())),
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(rpc:call(N1,wn_resource_layer,list_resources,[]))),
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(rpc:call(N2,wn_resource_layer,list_resources,[])))
              % ListA = lists:sort(wn_resource_layer:list_resources()),
              % ListB = lists:sort(rpc:call(N1,wn_resource_layer,list_resources,[])),
              % ListC = lists:sort(rpc:call(N2,wn_resource_layer,list_resources,[])),
              % ?assertEqual([ResourceA,ResourceC,ResourceB],ListA),
              % ?assertEqual([ResourceA,ResourceC,ResourceB],ListB),
              % ?assertEqual([ResourceA,ResourceC,ResourceB],ListC)
     end}.

register_restart_register([N1,N2]) ->
    {"Can Register, Restart and Register",
     fun() ->
             rpc:call(N1,wn_resource_layer,start_link,[]),
             rpc:call(N2,wn_resource_layer,start_link,[]),
             ResourceA = #wn_resource{name = "erlang R14",
                                      type = [{erlang,infinity}],
                                      resides = N1},
             ResourceB = #wn_resource{name = "os-x macbook pro",
                                      type = [{'os-x',1}],
                                      resides = N2},
             ResourceC = #wn_resource{name = "g++",
                                      type = [{'g++',1}],
                                      resides = node()},
             ok = wn_resource_layer:register(ResourceA),
             ok = wn_resource_layer:register(ResourceB),
             ok = wn_resource_layer:register(ResourceC),
             M = fun() -> lists:sort(wn_resource_layer:list_resources()) end,
             S1 = fun() -> lists:sort(rpc:call(N1,wn_resource_layer,list_resources,[])) end,
             S2 = fun() -> lists:sort(rpc:call(N2,wn_resource_layer,list_resources,[])) end,
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(wn_resource_layer:list_resources())),
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(rpc:call(N1,wn_resource_layer,list_resources,[]))),
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(rpc:call(N2,wn_resource_layer,list_resources,[]))),
             rpc:call(N1,wn_resource_layer,stop,[]),
             ?assertEqual([ResourceC,ResourceB],lists:sort(wn_resource_layer:list_resources())),
             ?assertEqual([ResourceC,ResourceB],lists:sort(rpc:call(N2,wn_resource_layer,list_resources,[]))),
             rpc:call(N2,wn_resource_layer,stop,[]),
             ?assertEqual([ResourceC],lists:sort(wn_resource_layer:list_resources())),
             {ok,_} = rpc:call(N1,wn_resource_layer,start_link,[]),
             {ok,_} = rpc:call(N2,wn_resource_layer,start_link,[]),
             ok = wn_resource_layer:register(ResourceA),
             ?assertEqual([ResourceA,ResourceC],lists:sort(wn_resource_layer:list_resources())),
             ok = wn_resource_layer:register(ResourceB),
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(wn_resource_layer:list_resources())),
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(rpc:call(N1,wn_resource_layer,list_resources,[]))),
             ?assertEqual([ResourceA,ResourceC,ResourceB],lists:sort(rpc:call(N2,wn_resource_layer,list_resources,[])))
             % M = fun() -> lists:sort(wn_resource_layer:list_resources()) end,
             % S1 = fun() -> lists:sort(rpc:call(N1,wn_resource_layer,list_resources,[])) end,
             % S2 = fun() -> lists:sort(rpc:call(N2,wn_resource_layer,list_resources,[])) end,
             % ?assertEqual([ResourceA,ResourceC,ResourceB],M()),
             % ?assertEqual([ResourceA,ResourceC,ResourceB],S1()),
             % ?assertEqual([ResourceA,ResourceC,ResourceB],S2()),
             % rpc:call(N1,wn_resource_layer,stop,[]),
             % ?assertEqual([ResourceC,ResourceB],M()),
             % ?assertEqual([ResourceC,ResourceB],S2()),
             % rpc:call(N2,wn_resource_layer,stop,[]),
             % ?assertEqual([ResourceC],M()),
             % {ok,_} = rpc:call(N1,wn_resource_layer,start_link,[]),
             % {ok,_} = rpc:call(N2,wn_resource_layer,start_link,[]),
             % ok = wn_resource_layer:register(ResourceA),
             % ?assertEqual([ResourceA,ResourceC],M()),
             % ok = wn_resource_layer:register(ResourceB),
             % ?assertEqual([ResourceA,ResourceC,ResourceB],M()),
             % ?assertEqual([ResourceA,ResourceC,ResourceB],S1()),
             % ?assertEqual([ResourceA,ResourceC,ResourceB],S2())
     end}.

register_deregister([N1,N2]) ->
    {"Can Register, Deregister and Register",
     fun() ->
             rpc:call(N1,wn_resource_layer,start_link,[]),
             rpc:call(N2,wn_resource_layer,start_link,[]),
             M = fun() -> lists:sort(wn_resource_layer:list_resources()) end,
             S1 = fun() -> lists:sort(rpc:call(N1,wn_resource_layer,list_resources,[])) end,
             S2 = fun() -> lists:sort(rpc:call(N2,wn_resource_layer,list_resources,[])) end,
             ResourceA = #wn_resource{name = "A",type = [{a,1}],resides = N1},
             ResourceB = #wn_resource{name = "B",type = [{b,2}],resides = N2},
             ResourceC = #wn_resource{name = "C",type = [{c,3}],resides = node()},
             ok = wn_resource_layer:register(ResourceA),
             ok = wn_resource_layer:register(ResourceB),
             ok = wn_resource_layer:register(ResourceC),
             ?assertEqual([ResourceA,ResourceB,ResourceC],M()),
             ?assertEqual([ResourceA,ResourceB,ResourceC],S1()),
             ?assertEqual([ResourceA,ResourceB,ResourceC],S2()),
             ?assertEqual(ok,wn_resource_layer:deregister(N1,"A")),
             ?assertEqual([ResourceB,ResourceC],M()),
             ?assertEqual([ResourceB,ResourceC],S1()),
             ?assertEqual([ResourceB,ResourceC],S2()),
             ?assertEqual(ok,wn_resource_layer:deregister(N2,"B")),
             ?assertEqual([ResourceC],M()),
             ?assertEqual([ResourceC],S1()),
             ?assertEqual([ResourceC],S2()),
             ?assertEqual(ok,wn_resource_layer:deregister(node(),"C")),
             ?assertEqual([],M()),
             ?assertEqual([],S1()),
             ?assertEqual([],S2()),
             ok = wn_resource_layer:register(ResourceA),
             ok = wn_resource_layer:register(ResourceB),
             ok = wn_resource_layer:register(ResourceC),
             ?assertEqual([ResourceA,ResourceB,ResourceC],M()),
             ?assertEqual([ResourceA,ResourceB,ResourceC],S1()),
             ?assertEqual([ResourceA,ResourceB,ResourceC],S2())
     end}.

%% -----------------------------------------------------------------
setup() ->
    {ok,_} = net_kernel:start([eunit_resource,shortnames]),
    erlang:set_cookie(node(),eunit),
    {ok,_} = wn_resource_layer:start_link().

cleanup(_) ->
    ok = net_kernel:stop(),
    ok = wn_resource_layer:stop().

distr_setup() ->
    setup(),
    Host = list_to_atom(inet_db:gethostname()),
    Args = " -pa "++hd(code:get_path())++" -setcookie eunit",
    {ok,N1} = slave:start(Host,n1,Args),
    {ok,N2} = slave:start(Host,n2,Args),
    rpc:call(N1,net_kernel,connect_node,[N2]),
    [N1,N2].

distr_cleanup([N1,N2]) ->
    rpc:call(N1,wn_resource_layer,stop,[]),
    rpc:call(N2,wn_resource_layer,stop,[]),
    slave:stop(N1),
    slave:stop(N2),
    cleanup(nothing).
