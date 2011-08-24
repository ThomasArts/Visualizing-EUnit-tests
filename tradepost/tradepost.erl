%%%-------------------------------------------------------------------
%%% @author Gianfranco <zenon@zen.home>
%%% @copyright (C) 2010, Gianfranco
%%% Created :  2 Sep 2010 by Gianfranco <zenon@zen.home>
%%%-------------------------------------------------------------------
-module(tradepost).
-behaviour(gen_fsm).

%% API
-export([start_link/0,introspection_statename/1,introspection_loopdata/1,
         stop/1,seller_identify/2,seller_insertitem/3,withdraw_item/2]).

%% States
-export([pending/2,pending/3,item_received/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).
-record(state, {object,cash,seller,buyer,time}).

%%% API
start_link() -> gen_fsm:start_link(?MODULE, [], []).

introspection_statename(TradePost) ->
    gen_fsm:sync_send_all_state_event(TradePost,which_statename).
introspection_loopdata(TradePost) ->
    gen_fsm:sync_send_all_state_event(TradePost,which_loopdata).
stop(Pid) -> gen_fsm:sync_send_all_state_event(Pid,stop).

seller_identify(TradePost,Password) ->
    gen_fsm:sync_send_event(TradePost,{identify_seller,Password}).
seller_insertitem(TradePost,Item,Password) ->
    gen_fsm:sync_send_event(TradePost,{insert,Item,Password}).

withdraw_item(TradePost,Password) ->
    gen_fsm:sync_send_event(TradePost,{withdraw,Password}).

%%--------------------------------------------------------------------
pending(_Event,LoopData) -> {next_state,pending,LoopData}.

pending({identify_seller,Password},_Frm,LoopD = #state{seller=Password}) ->
    {reply,ok,pending,LoopD};
pending({identify_seller,Password},_Frm,LoopD = #state{seller=undefined}) ->
    {reply,ok,pending,LoopD#state{seller=Password}};
pending({identify_seller,_},_,LoopD) ->
    {reply,error,pending,LoopD};

pending({insert,Item,Password},_Frm,LoopD = #state{seller=Password}) ->
    {reply,ok,item_received,LoopD#state{object=Item}};
pending({insert,_,_},_Frm,LoopD) ->
    {reply,error,pending,LoopD}.

item_received({withdraw,Password},_Frm,LoopD = #state{seller=Password}) ->
    {reply,ok,pending,LoopD#state{object=undefined}};
item_received({withdraw,_},_Frm,LoopD) ->
    {reply,error,item_received,LoopD}.

%%--------------------------------------------------------------------
handle_sync_event(which_statename, _From, StateName, LoopData) ->
    {reply, StateName, StateName, LoopData};
handle_sync_event(which_loopdata, _From, StateName, LoopData) ->
    {reply,tl(tuple_to_list(LoopData)),StateName,LoopData};
handle_sync_event(stop,_From,_StateName,LoopData) ->
    {stop,normal,ok,LoopData};
handle_sync_event(_E,_From,StateName,LoopData) ->
    {reply,ok,StateName,LoopData}.

%%--------------------------------------------------------------------
init([]) -> {ok, pending, #state{}}.
handle_event(_Event, StateName, State) ->{next_state, StateName, State}.
handle_info(_Info, StateName, State) -> {next_state, StateName, State}.
terminate(_Reason, _StateName, _State) -> ok.
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.
