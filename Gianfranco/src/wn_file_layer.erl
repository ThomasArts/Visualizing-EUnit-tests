%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.local>
%%% @copyright (C) 2010, Gianfranco
%%% Created : 19 Dec 2010 by Gianfranco <zenon@zen.local>
%%%-------------------------------------------------------------------
-module(wn_file_layer).
-behaviour(gen_server).
-include("include/worker_net.hrl"). 

%% API
-export([start_link/1,add_file/1,list_files/0,stop/0,
        retrieve_file/2,delete_file/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        { node_root :: string()
         }).

%%%===================================================================
%%% API
%%%===================================================================
start_link(NodeRoot) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, NodeRoot, []).

-spec(add_file(#wn_file{}) -> ok | {error,term()}).
add_file(WnFile) ->
    case gen_server:call(?MODULE,{add_file,WnFile}) of
        {ok,{Pid,Ref}} ->
            {ok,IoDev} = file:open(WnFile#wn_file.file,[read,binary]),
            Pid ! {Ref,self(),IoDev},
            receive
                {Ref,Result} -> Result
            end;
        Error -> Error
    end.

-spec(list_files() -> [#wn_file{}]).
list_files() ->
   gen_server:call(?MODULE,list_all_files).

-spec(stop() -> ok).
stop() ->
    gen_server:call(?MODULE,stop).

-spec(retrieve_file(node(),string()) -> {ok,string()} | {error,term()}).
retrieve_file(Node,Id) ->
    case gen_server:call(?MODULE,{retrieve_file,Node,Id}) of
        {ok,{ReadDev,Name}} ->
            retrieve_file(ReadDev,Name,file:open(Name,[write,binary]));
        X -> X
    end.
retrieve_file(ReadDev,Name,{ok,WriteDev}) ->
    case receive_file_client(WriteDev,ReadDev) of
        ok -> {ok,Name};
        Err -> Err
    end;
retrieve_file(ReadDev,_Name,Err) ->
    file:close(ReadDev),
    Err.

-spec(delete_file(node(),string()) -> ok | {error,term()}).
delete_file(Node,Id) ->
    gen_server:call(?MODULE,{delete_file,Node,Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(NodeRoot) ->
    ok = filelib:ensure_dir(NodeRoot++atom_to_list(node())++"/"),
    {ok, #state{node_root = NodeRoot}}.

handle_call(stop,_From,State) ->
    {stop,normal,ok,State};

handle_call(list_all_files,From,State) ->
    spawn_link(file_collector(From)),
    {noreply,State};

handle_call(list_files,_From,State) ->
    PropList = check_files(State#state.node_root),
    {reply,[V || {_,V} <- PropList],State};

handle_call({delete_file,Node,Id},From,State) ->
    case {node() == Node, lists:member(Node,nodes())} of
        {true,false} ->
            {reply,try_delete_file(Id,State),State};
        {false,true} ->
            case rpc:call(Node,erlang,whereis,[?MODULE]) of
                undefined -> {reply,{error,noresides},State};
                _Pid ->
                    gen_server:cast({?MODULE,Node},{delete_file,From,Id}),
                    {noreply,State}
            end;
        {false,false} ->
            {reply,{error,noresides},State}
    end;

handle_call({retrieve_file,Node,Id},From,State) ->
    case {node() == Node, lists:member(Node,nodes())} of
        {true,false} ->
            Result = try_retrieve(Id,State),
            {reply,Result,State};
        {false,true} ->
            case rpc:call(Node,erlang,whereis,[?MODULE]) of
                undefined -> {reply,{error,noresides},State};
                _Pid ->
                    gen_server:cast({?MODULE,Node},{retrieve_file,From,Id}),
                    {noreply,State}
            end;
        {false,false} ->
            {reply,{error,noresides},State}
    end;

handle_call({add_file,WnFile}, From, State) ->
    #wn_file{resides=Node} = WnFile,
    case {Node == node(),lists:member(Node,nodes())} of
        {true,false} ->
            Result = try_add(WnFile,State),
            {reply,Result,State};
        {false,true} ->
            case rpc:call(Node,erlang,whereis,[?MODULE]) of
                undefined -> {reply,{error,noresides},State};
                _Pid ->
                    gen_server:cast({?MODULE,Node},{add_file,From,WnFile}),
                    {noreply,State}
            end;
        {false,false} ->
            {reply,{error,noresides},State}
    end.

handle_cast({delete_file,From,Id},State) ->
    Result = try_delete_file(Id,State),
    gen_server:reply(From,Result),
    {noreply,State};

handle_cast({retrieve_file,From,Id},State) ->
    Result = try_retrieve(Id,State),
    gen_server:reply(From,Result),
    {noreply,State};

handle_cast({add_file,From,WnFile},State) ->
    Result = try_add(WnFile,State),
    gen_server:reply(From,Result),
    {noreply,State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_files(Root) ->
    ok = filelib:ensure_dir(Root),
    Path = Root++atom_to_list(node())++"/",
    {ok,NameDirs} = file:list_dir(Path),
    lists:foldl(
      fun(Dir,Acc) ->
              {ok,[File]}  = file:list_dir(Path++Dir),
              [{Dir,#wn_file{id = Dir,
                             file = Path++Dir++"/"++File,
                             resides = node()}} | Acc]
      end,[],NameDirs).

receive_file(Ref,WriteDev) ->
    fun() ->
            receive
                {Ref,Pid,ReadDev} ->
                    receive_file(Ref,Pid,WriteDev,ReadDev)
            end
    end.

receive_file(Ref,Pid,WriteDev,ReadDev) ->
    Res = close_transfer(WriteDev,ReadDev,
                         transfer(WriteDev,ReadDev)),
    Pid ! {Ref,Res}.

receive_file_client(WriteDev,ReadDev) ->
    close_transfer(WriteDev,ReadDev,
                   transfer(WriteDev,ReadDev)).

close_transfer(WriteDev,ReadDev,TransferResult) ->
    case
        lists:dropwhile(fun(X) -> X == ok end,
                        [TransferResult,
                         file:close(WriteDev),
                         file:close(ReadDev)]) of
        [] -> ok;
        [X|_] -> X
    end.

-spec(transfer(pid(),pid()) -> ok | {error,term()}).
transfer(WriteDev,ReadDev) ->
    transfer(WriteDev,ReadDev,file:read(ReadDev,1024)).

transfer(WriteDev,ReadDev,{ok,Data}) ->
    case file:write(WriteDev,Data) of
        ok -> transfer(WriteDev,ReadDev,file:read(ReadDev,1024));
        Err -> Err
    end;
transfer(_WriteDev,_ReadDev,eof) -> ok;
transfer(_WriteDev,_ReadDev,Err) -> Err.

file_collector(From) ->
    Nodes = [node()|nodes()],
    fun() ->
            Res =
                lists:foldl(
                  fun(Node,Acc) ->
                          case rpc:call(Node,erlang,whereis,[?MODULE]) of
                              undefined -> Acc;
                              _Pid ->
                                  gen_server:call({?MODULE,Node},
                                                  list_files)++Acc
                        end
                  end,[],Nodes),
            gen_server:reply(From,Res)
    end.

try_delete_file(Id,State) ->
    PropList = check_files(State#state.node_root),
    case proplists:lookup(Id,PropList) of
        none ->
            {error,noexists};
        {Id,WnFile} ->
            List = [ file:delete(WnFile#wn_file.file),
                     file:del_dir(State#state.node_root++
                                  atom_to_list(node())++"/"++
                                  WnFile#wn_file.id++"/") ],
            case lists:dropwhile(fun(X) -> X == ok end,List) of
                [] -> ok;
                [X|_] -> X
            end
    end.

try_retrieve(Id,State) ->
    PropList = check_files(State#state.node_root),
    case proplists:lookup(Id,PropList) of
        none -> {error,noexists};
        {Id,WnFile} ->
            Path = WnFile#wn_file.file,
            {ok,ReadDev} = file:open(Path,[read,binary]),
            {ok,{ReadDev,filename:basename(Path)}}
    end.

try_add(WnFile,State) ->
    Path = State#state.node_root++"/"++
        atom_to_list(node())++"/"++WnFile#wn_file.id++"/"++
        filename:basename(WnFile#wn_file.file),
    case filelib:is_file(Path) of
        true -> {error,exists};
        false ->
            ok = filelib:ensure_dir(Path),
            {ok,WriteDev} = file:open(Path,[write,binary]),
            Ref = make_ref(),
            Pid = spawn(receive_file(Ref,WriteDev)),
            {ok,{Pid,Ref}}
    end.
