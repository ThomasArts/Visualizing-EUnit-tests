%%% File    : rfsa_eqc.erl
%%% Author  : Thomas Arts <>
%%% Description : QuickCheck properties for rfsa.erl
%%% Created :  9 Sep 2010 by Thomas Arts <>

-module(rfsa_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

word() ->
  list(elements(language())).

language() ->
  [$0,$1].

set(G) ->
  ?LET(S,list(G),lists:usort(S)).

sign() ->
  elements([pos,neg]).

signedset(G) ->
  ?LET(USet,set(G),
       ?LET(Signs,vector(length(USet),sign()),
            lists:zip(USet,Signs))).


%% automata() ->
%%   ?LET({States,Events},{?SUCHTHAT(S,set(elements([a,b,c,d,e,f,g])),length(S) >= 2),
%%                         non_empty(set(elements([$0,$1,$2,$3])))},
%%        ?LET({BadState,Trans},{elements(States),non_empty(transitions(States,Events))},
%%             {States,elements(States--[BadState]),BadState,Events,
%%              [Tr || Tr = {From,_,_} <- lists:usort(Trans), From /= BadState]}
%%            )).

automata() ->
  ?LET({States,Events}, {set(elements([a,b,c,d,e,f,g])),
                         non_empty(set(elements([x,w,y,z])))},
       ?LET(Trs, transitions(Events,States),
       {[init,bad] ++ States,init,bad,Events,Trs})).

transitions(Events, States) ->
    ?LET(Trs, [{init,elements(Events),oneof([bad | States])} |
	       normal_transitions([init|States], Events, [bad,init|States])],
	 determinize(Trs)).

%% transitions(States,Events) ->
%%   list({elements(States),elements(Events),elements(States)}).
normal_transitions(Froms,Events,Tos) ->
  ?LET(Trs, list({elements(Froms),elements(Events),elements(Tos)}),
       lists:usort(Trs)).

determinize([]) -> [];
determinize([Tr|Trs]) ->
  determinize([Tr],Trs).
determinize(Trs,[]) ->
  Trs;
determinize(DTrs,[Tr={From,Ev,_To}|Trs]) ->
  case [X || X = {From2,Ev2,_To2} <- DTrs,
             From2 == From,
             Ev2 == Ev] of
    [] -> determinize([Tr|DTrs],Trs);
    _ -> determinize(DTrs,Trs)
  end.

subset(Set) ->
  ?LET(SubSet,list(elements(Set)),lists:usort(SubSet)).

  %% [ Tr || Tr = {From, Ev, To} <- Trs,
  %%         [] == [Tr2 || Tr2 = {From2,Ev2,To2} <- Trs,
  %%                       From2 == From,
  %%                       Ev2 == Ev,
  %%                       To2 /= To]].

automata_sample({_States,_StartState,_FinalStates,_Events,_Transitions} = Automata) ->
  ?LET(Strings,non_empty(list(?SIZED(Size,?LET(N,choose(1,(Size div 3)+1),random_walk(Automata,init,N))))),
       begin
         UStrings = lists:usort(Strings),
         [ case fix_accepted(Automata,String) of
             true -> {String,pos};
             false -> {String,neg}
           end || String<-UStrings]
       end).

random_walk(_Automata,_State,0) ->
  [];
random_walk({_States,_StartState,_FinalStates,_Events,Transitions} = Automata,State,N) ->
  Alts = [{Event,To} || {From,Event,To} <- Transitions,
                        From == State],
  case Alts of
    [] -> [];
    _ -> ?LET({Event,NextState}, elements(Alts), [Event | random_walk(Automata,NextState,N-1)])
  end.

%% take_steps(_,_,0) ->
%%   "";
%% take_steps({_States,_StartState,_FinalStates,_Events,Transitions} = Automata,State,N) ->
%%   Alts = [ {X,To} || {From,X,To} <- Transitions,
%%                      From == State],
%%   case Alts of
%%     [] -> "";
%%     _ ->  ?LET({Event,NextState}, elements(Alts), [Event | take_steps(Automata,NextState,N-1)])
%%   end.

fix_accepted({States,StartState,BadState,Events,Delta}, String) ->
  accepted({States,StartState,[BadState],Events,Delta}, String).

accepted({_States,StartState,BadState,_Events,_} = Automata, String) ->
  ReachableStates = reachable(Automata,[StartState], String),
  case ReachableStates of
    [] -> throw(outOfAutomata);
    _ -> case BadState of
	   [BS] -> not lists:member(BS, ReachableStates);
           _ -> true
         end
  end.

accepted_severalbadstates({_States,StartState,BadStates,_Events,_} = Automata, String) ->
	ReachableStates = reachable(Automata,[StartState], String),
	case ReachableStates of
		[] -> throw(outOfAutomata);
		_ -> case BadStates -- ReachableStates of
				 BadStates -> true;
				 _ -> false
			 end
	end.
  
weak_accepted({_States,StartState,BadState,_Events,_} = Automata, String) ->
  ReachableStates = reachable(Automata,[StartState], String),
  case ReachableStates of
    [] -> false;
    _ -> case BadState of
	   [BS] -> not lists:member(BS, ReachableStates);
           _ -> true
         end
  end.

reachable(_Automata,RStates,[]) ->
  RStates;
reachable({_States,_StartState,_FinalStates,_Events,Transitions} = Automata,RStates,[Char| String]) ->
  NewRStates =
    lists:usort(
      lists:foldl(fun ({From,Ch,To},NewStates) when Ch==Char ->
                      case lists:member(From,RStates) of
                        true ->
                          [To| NewStates];
                        false ->
                          NewStates
                      end;
                      (_,NewStates) ->
                      NewStates
                  end,[],Transitions)),
  reachable(Automata,NewRStates,String).

prop_automata_check() ->
  ?FORALL(Automata,automata(),
          ?FORALL(S,automata_sample(Automata),
                  [ 1 || {X,neg}<-S, X=/=[]] == [])).

prop_bluefringe() ->
  ?FORALL(Automata,automata(),
    ?FORALL(S,automata_sample(Automata),
      begin
        Pos = [Seq || {Seq,pos} <- S],
        Neg = [Seq || {Seq,neg} <- S],
        bluefringe:rei(bluefringe:qsm({Pos,Neg})),
	
        ?WHENFAIL(io:format("POS: ~p\nNEG: ~p\n",[Pos,Neg]),true)
      end)).

prop_qsmtool() ->
  ?FORALL(Automata,?SIZED(Size,resize(Size*5,automata())),
    ?FORALL(S,?SIZED(Size,resize(Size*5,automata_sample(Automata))),
      begin
        Pos = [Seq || {Seq,pos} <- S],
        Neg = [Seq || {Seq,neg} <- S],
        qsm_wrapper:run(Pos,Neg),
        ?WHENFAIL(io:format("POS: ~p\nNEG: ~p\n",[Pos,Neg]),true)
      end)).

prop_compare() -> 
  ?FORALL(Automata,noshrink(?SIZED(Size,resize(Size*5,automata()))),
    ?FORALL(S,?SIZED(Size,resize(Size*5,automata_sample(Automata))),
      begin
        Pos = [Seq || {Seq,pos} <- S],
        Neg = [Seq || {Seq,neg} <- S],
        QsmRes = qsm_wrapper:run(Pos,Neg),
        BlueRes = automata:automataToTuple(bluefringe:rei(bluefringe:qsm({Pos,Neg}))),
        ?WHENFAIL(io:format("QSM: ~p\nBluefringe: ~p\nInput: ~p\n",[QsmRes,BlueRes,{Pos,Neg}]),
                  cmp_automata(QsmRes,BlueRes))
      end)).

prop_statistic() ->
  ?FORALL(Automata,noshrink(?SIZED(Size,resize(Size*5,automata()))),
    ?FORALL(S,?SIZED(Size,resize(Size*5,automata_sample(Automata))),
      begin
        Pos = [Seq || {Seq,pos} <- S],
        Neg = [Seq || {Seq,neg} <- S],
        QsmRes = qsm_wrapper:run(Pos,Neg),
        BlueRes = automata:automataToTuple(bluefringe:rei(bluefringe:qsm({Pos,Neg}))),
        collect(which_automata(QsmRes,BlueRes), true)
      end)).

reverse_abstraction(Trace, N) -> [{E, N} || E <- Trace].

prop_abstraction() ->
	?FORALL(Automata,automata(),
			?FORALL(S,automata_sample(Automata),
					begin
						Abstraction = fun ({A, _B}) -> A end,
						Pos1 = [reverse_abstraction(Seq, 1) || {Seq,pos} <- S],
						Pos2 = [reverse_abstraction(Seq, 2) || {Seq,pos} <- S],
						Neg1 = [reverse_abstraction(Seq, 1) || {Seq,neg} <- S],
						Pos = [Seq || {Seq,pos} <- S],
						Neg = [Seq || {Seq,neg} <- S],
						BlueRes = bluefringe:rei(bluefringe:qsm({Pos1 ++ Pos2, Neg1}, Abstraction)),
						BlueRes2 = bluefringe:rei(bluefringe:qsm({Pos, Neg})),
						?WHENFAIL(io:format("Input: ~p, ~p~n", [{Pos1 ++ Pos2, Neg1},{Pos, Neg}]), equals(BlueRes, BlueRes2))
					end)).
			
customAccepted(Auto) -> fun (X) -> accepted(Auto, X) end.
customAccepted_severalbadstates(Auto) -> fun (X) -> accepted_severalbadstates(Auto, X) end.
customWeakAccepted(Auto) -> fun (X) -> weak_accepted(Auto, X) end.

neg(A) -> fun (X) -> not A(X) end.

prop_check() ->
  ?FORALL(Automata,noshrink(?SIZED(Size,resize(Size*5,automata()))),
    ?FORALL(S,?SIZED(Size,resize(Size*5,automata_sample(Automata))),
      begin
        Pos = [Seq || {Seq,pos} <- S],
        Neg = [Seq || {Seq,neg} <- S],
% Debug line
%        io:format("TRY:Pos: ~p\nNeg: ~p\n", [Pos, Neg]),
	Auto = bluefringe:rei(bluefringe:qsm({Pos,Neg})),
	Tuple = automata:automataToTuple(Auto),
	Accepted = customAccepted(Tuple),
	?WHENFAIL(io:format("QSM: ~p\nPos: ~p\nNeg: ~p\n", [Auto, Pos, Neg]),
		  (lists:filter(neg(Accepted), Pos) ++ lists:filter(Accepted, Neg)) =:= [])
      end)).

demux_event([{Ori, Events, Dest}|Tail]) ->
  demux_event2([{list_to_atom(Ori), Events, list_to_atom(Dest)}|Tail]);
demux_event([]) -> [].
demux_event2([{Ori, [Eve, Other|ETail], Dest}|Tail]) ->
  [{Ori, Eve, Dest}|demux_event2([{Ori, [Other|ETail], Dest}|Tail])];
demux_event2([{Ori, [Eve], Dest}|Tail]) -> [{Ori, Eve, Dest}|demux_event(Tail)];
demux_event2([]) -> [].

%% This property cannot be fixed because of lack of output from StateChum
%%prop_check_qsm() ->
%%?FORALL(Automata,noshrink(?SIZED(Size,resize(Size*5,automata()))),
%%  ?FORALL(S,?SIZED(Size,resize(Size*5,automata_sample(Automata))),
%%     begin
%%      Pos = [Seq || {Seq,pos} <- S],
%%      Neg = [Seq || {Seq,neg} <- S],
%%	{Q,Q0,Delta} = qsm_wrapper:run(Pos,Neg),
%%	Tuple = {lists:map(fun list_to_atom/1,Q),list_to_atom(Q0),[],[a,b,c,d,e,f,g,x,w,y,z],demux_event(Delta)},
%%	Accepted = customWeakAccepted(Tuple),
%%	?WHENFAIL(io:format("QSM: ~p\nPos: ~p\nNeg: ~p\n", [Tuple, Pos, Neg]),
%%		  (lists:filter(neg(Accepted), Pos) ++ lists:filter(Accepted, Neg)) =:= [])
%%      end)).

cmp_automata({QStates,_QInitState,_QTrans},{BStates,_BInitState,BFailState,_BAlpha,_BTrans}) ->
  length(QStates) >= length(BStates) - length(BFailState).

which_automata({QStates,_QInitState,_QTrans},{BStates,_BInitState,BFailState,_BAlpha,_BTrans}) ->
  case {length(QStates), length(BStates) - length(BFailState)} of
    {SC, QSM} when SC > QSM -> qsm;
    {SC, QSM} when QSM > SC -> sc;
    _ -> draw
  end.

prop_check_apta() ->
  ?FORALL(Automata,noshrink(?SIZED(Size,resize(Size*5,automata()))),
    ?FORALL(S,?SIZED(Size,resize(Size*5,automata_sample(Automata))),
      begin
        Pos = [Seq || {Seq,pos} <- S],
        Neg = [Seq || {Seq,neg} <- S],
% Debug line
%        io:format("TRY:Pos: ~p\nNeg: ~p\n", [Pos, Neg]),
	Auto = bluefringe:rei(bluefringe_apta:generateApta({Pos,Neg})),
	Tuple = automata:automataToTuple(Auto),
	Accepted = customAccepted_severalbadstates(Tuple),
	?WHENFAIL(io:format("QSM: ~p\nPos: ~p\nNeg: ~p\n", [Auto, Pos, Neg]),
		  (lists:filter(neg(Accepted), Pos) ++ lists:filter(Accepted, Neg)) =:= [])
      end)).
