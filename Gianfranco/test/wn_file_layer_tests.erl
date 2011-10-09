%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2010, Gianfranco
%%% Created : 19 Dec 2010 by Gianfranco <zenon@zen.local>
-module(wn_file_layer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/worker_net.hrl").

-define(NODE_ROOT,
        "/Users/simonthompson/protest/Work/Visualizing-EUnit-tests/Gianfranco/node_root/").

file_layer_local_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Can store file locally", fun store_locally/0},
      {"Can retrieve files locally",fun store_retrieve_locally/0},
      {"Can delete files locally",fun store_delete_locally/0}
      ]}.

file_layer_distributed_test_() ->
   {foreach,
     fun distr_setup/0,
     fun distr_cleanup/1,
     [
      fun can_store_distributed/1,
      fun can_retrieve_distributed/1,
      fun can_delete_distributed/1,
      fun must_retain/1
      ]}.

must_retain([N1,N2]) ->
    {"Must retain information between node kill and node restart",
     fun() ->
             ?assertMatch({ok,_},rpc:call(N1,wn_file_layer,start_link,[?NODE_ROOT])),
             ?assertMatch({ok,_},rpc:call(N2,wn_file_layer,start_link,[?NODE_ROOT])),
             Path = create_file_at(?NODE_ROOT),
             FileA = #wn_file{id = "File1",file = Path,resides = N1},
             FileB = #wn_file{id = "File2",file = Path,resides = N2},
             ok = wn_file_layer:add_file(FileA),
             ok = wn_file_layer:add_file(FileB),
             [ResA,ResB] = wn_file_layer:list_files(),
             ?assertEqual(filename:basename(Path),filename:basename(ResA#wn_file.file)),
             ?assertEqual(filename:basename(Path),filename:basename(ResB#wn_file.file)),
             % Kill N1 and N2
             slave:stop(N1),
             slave:stop(N2),
             ?assertEqual([],wn_file_layer:list_files()),
             % Restart and check
             Host = list_to_atom(inet_db:gethostname()),
             Args = " -pa "++hd(code:get_path())++" -setcookie eunit",
             {ok,N1} = slave:start(Host,n1,Args),
             {ok,N2} = slave:start(Host,n2,Args),
             rpc:call(N1,net_kernel,connect_node,[N2]),
             ?assertMatch({ok,_},rpc:call(N1,wn_file_layer,start_link,[?NODE_ROOT])),
             ?assertMatch({ok,_},rpc:call(N2,wn_file_layer,start_link,[?NODE_ROOT])),
             ?assertEqual([ResA,ResB],wn_file_layer:list_files())
     end}.

can_delete_distributed([N1,N2]) ->
    {"Can delete file distributed",
     fun() ->
             ?assertMatch({ok,_},rpc:call(N1,wn_file_layer,start_link,[?NODE_ROOT])),
             ?assertMatch({ok,_},rpc:call(N2,wn_file_layer,start_link,[?NODE_ROOT])),
             Path = create_file_at(?NODE_ROOT),
             Id = "AddedFile1",
             File = #wn_file{id = Id,file = Path,resides = N1},
             ok = wn_file_layer:add_file(File),
             ?assertEqual(ok,wn_file_layer:delete_file(N1,Id)),
             ?assertEqual([],wn_file_layer:list_files()),
             % Check change in file-system
             ExpectedPath = ?NODE_ROOT++"/"
                 ++atom_to_list(node())
                 ++"/"++Id++"/"++filename:basename(Path),
             ?assertEqual(false,filelib:is_file(ExpectedPath)),
             ?assertEqual(false,filelib:is_dir(?NODE_ROOT++"/"
                                               ++atom_to_list(N1)++"/"
                                               ++Id++"/"))
     end}.

can_retrieve_distributed([N1,N2]) ->
    {"Can retrieve file distributed",
     fun() ->
             ?assertMatch({ok,_},rpc:call(N1,wn_file_layer,start_link,[?NODE_ROOT])),
             ?assertMatch({ok,_},rpc:call(N2,wn_file_layer,start_link,[?NODE_ROOT])),
             Path = create_file_at(?NODE_ROOT),
             Id = "AddedFile1",
             File = #wn_file{id = Id,file = Path,resides = N1},
             ok = wn_file_layer:add_file(File),
             {ok,OriginalData} = file:read_file(Path),
             ?assertEqual(ok,file:delete(Path)),
             % Retrieve and see change in file system
             {ok,FileName} =  wn_file_layer:retrieve_file(N1,Id),
             ?assertEqual(true,filelib:is_file(FileName)),
             {ok,NewData} = file:read_file(FileName),
             ?assertEqual(OriginalData,NewData),
             ?assertEqual(filename:basename(Path),FileName),
             file:delete(FileName)
     end}.

can_store_distributed([N1,N2]) ->
    {"Can store file distributed",
     fun() ->
             ?assertMatch({ok,_},rpc:call(N1,wn_file_layer,start_link,[?NODE_ROOT])),
             ?assertMatch({ok,_},rpc:call(N2,wn_file_layer,start_link,[?NODE_ROOT])),
             Path = create_file_at(?NODE_ROOT),
             Id = "AddedFile1",
             File = #wn_file{id = Id,file = Path,resides = N1},
             ok = wn_file_layer:add_file(File),
             [Res] = wn_file_layer:list_files(),
             [Res2] = rpc:call(N1,wn_file_layer,list_files,[]),
             [Res3] = rpc:call(N2,wn_file_layer,list_files,[]),
             ?assertEqual(filename:basename(Path),filename:basename(Res#wn_file.file)),
             ?assertEqual(Id,Res#wn_file.id),
             ?assertEqual(N1,Res#wn_file.resides),
             ?assertEqual(Res,Res2),
             ?assertEqual(Res2,Res3),
             % Check change in file-system
             ExpectedPath = ?NODE_ROOT++"/"++atom_to_list(N1)++"/"++
                 Id++"/"++filename:basename(Path),
             ?assertEqual(true,filelib:is_file(Path)),
             ?assertEqual(true,filelib:is_file(ExpectedPath))
     end}.

store_locally() ->
    Path = create_file_at(?NODE_ROOT),
    Id = "AddedFile1",
    File = #wn_file{id = Id,file = Path,resides = node()},
    ok = wn_file_layer:add_file(File),
    [Res] = wn_file_layer:list_files(),
    ?assertEqual(filename:basename(Path),filename:basename(Res#wn_file.file)),
    ?assertEqual(Id,Res#wn_file.id),
    ?assertEqual(node(),Res#wn_file.resides),
    % Check change in file-system
    ExpectedPath = ?NODE_ROOT++"/"++atom_to_list(node())++"/"++
        Id++"/"++filename:basename(Path),
    ?assertEqual(true,filelib:is_file(Path)),
    ?assertEqual(true,filelib:is_file(ExpectedPath)).

store_retrieve_locally() ->
    Path = create_file_at(?NODE_ROOT),
    Id = "AddedFile1",
    File = #wn_file{id = Id,file = Path,resides = node()},
    ok = wn_file_layer:add_file(File),
    {ok,OriginalData} = file:read_file(Path),
    ?assertEqual(ok,file:delete(Path)),
    % Retrieve and see change in file system
    {ok,FileName} =  wn_file_layer:retrieve_file(node(),Id),
    ?assertEqual(true,filelib:is_file(FileName)),
    {ok,NewData} = file:read_file(FileName),
    ?assertEqual(OriginalData,NewData),
    ?assertEqual(filename:basename(Path),FileName),
    file:delete(FileName).

store_delete_locally() ->
    Path = create_file_at(?NODE_ROOT),
    Id = "AddedFile1",
    File = #wn_file{id = Id,file = Path,resides = node()},
    ok = wn_file_layer:add_file(File),
    ?assertEqual(ok,wn_file_layer:delete_file(node(),Id)),
    ?assertEqual([],wn_file_layer:list_files()),
    % Check change in file-system
    ExpectedPath = ?NODE_ROOT++"/"
        ++atom_to_list(node())
        ++"/"++Id++"/"++filename:basename(Path),
    ?assertEqual(false,filelib:is_file(ExpectedPath)),
    ?assertEqual(false,filelib:is_dir(?NODE_ROOT++"/"
                                      ++atom_to_list(node())++"/"
                                      ++Id++"/")).

%%------------------------------------------------------------------
create_file_at(X) ->
    Path = X++"EUnitFile",
    ok = filelib:ensure_dir(X),
    ok = file:write_file(Path,<<1,2,3>>),
    Path.

clean_up(X) ->
    case filelib:is_dir(X) of
        true ->
            {ok,Files} = file:list_dir(X),
            lists:foreach(
              fun(File) ->
                      clean_up(X++"/"++File)
              end,Files),
            file:del_dir(X);
        false ->
            ok = file:delete(X)
    end.

setup() ->
    {ok,_} = net_kernel:start([eunit_resource,shortnames]),
    erlang:set_cookie(node(),eunit),
    {ok,_} = wn_file_layer:start_link(?NODE_ROOT).

cleanup(_) ->
    clean_up(?NODE_ROOT),
    ok = net_kernel:stop(),
    ok = wn_file_layer:stop().

distr_setup() ->
    setup(),
    Host = list_to_atom(inet_db:gethostname()),
    Args = " -pa "++hd(code:get_path())++" -setcookie eunit",
    {ok,N1} = slave:start(Host,n1,Args),
    {ok,N2} = slave:start(Host,n2,Args),
    rpc:call(N1,net_kernel,connect_node,[N2]),
    [N1,N2].

distr_cleanup([N1,N2]) ->
    rpc:call(N1,wn_file_layer,stop,[]),
    rpc:call(N2,wn_file_layer,stop,[]),
    slave:stop(N1),
    slave:stop(N2),
    cleanup(nothing).
