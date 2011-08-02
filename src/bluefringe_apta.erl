%%%-------------------------------------------------------------------
%%% File    : apta.erl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Implements the functions related to APTA's.
%%%
%%% Created : 26 Oct 2010
%%% Modified: 31 May 2011 by Thomas Arts
%%%-------------------------------------------------------------------
-module(bluefringe_apta).

%% API
-export([generateApta/1, generateApta/2]).
-include("../include/visualize.hrl").

%% Automaton generation data
%% lastState, foundAlphabet, foundTransitions, rejectionStates, acceptingStates
-record(agd, {lastSt = 0, alph = [], tr = [], rSt = [], aSt = []}).



%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: generateApta(SetOfRules)
%% Description: Takes a set of rules S = {S+, S-} and returns the APTA
%%--------------------------------------------------------------------

generateApta(PosNeg) ->
  generateApta(PosNeg,fun(E) -> E end).

generateApta({Sp, Sm},Map) ->
  WrappedMap = fun(E) -> {Map(E), [E]} end,
  Alphabet = lists:usort([Map(Event) || Event <- lists:append(Sp ++ Sm)]),
  ExpAgd = breathfirst(#agd{lastSt=1}, 0, 
                       [Trace ++ [pos] || Trace <- Sp] ++ [Trace ++ [neg] || Trace <- Sm], WrappedMap),
  % check disjunction accepting and rejecting states
  case ExpAgd#agd.aSt -- (ExpAgd#agd.aSt -- ExpAgd#agd.rSt) of
    [] ->
      #fa{st = lists:seq(0,ExpAgd#agd.lastSt-1),
          alph = Alphabet, iSt = 0, tr = ExpAgd#agd.tr,
          fSt = ExpAgd#agd.rSt};
    _ ->
      exit(both_pos_and_neg)
  end.

breathfirst(Apta, _State, [], _Map) ->
  Apta;
breathfirst(Apta, State, Traces, Map) ->
  {Accept, Ts1} = 
    lists:partition(fun(Trace) -> Trace == [pos] end, 
                    Traces),
  {Reject, Ts2} = 
    lists:partition(fun(Trace) -> Trace == [neg] end, 
                    Ts1),
  SameHeads = 
    splitonhead(lists:usort([Map(E) || [E|_]<-Ts2]),
                Ts2,Map),
  {NextState,Transitions} =
    lists:foldl(fun({Hd,Tls},{NS,Trs}) ->
                    {NS+1,[{Hd,NS,Tls}|Trs]}
                end,{Apta#agd.lastSt,[]},SameHeads),
  NewApta =
    Apta#agd{aSt = ifadd(Apta#agd.aSt,Accept,State),
             rSt = ifadd(Apta#agd.rSt,Reject,State),
             lastSt = NextState
            },
  lists:foldr(fun({Hd,NS,Tls},A) ->
                  NewA = [{State,Hd,NS}|A#agd.tr],
                  breathfirst(A#agd{tr = NewA},
                              NS, Tls, Map)
              end,NewApta,Transitions).

st(Tuple) -> hd(tuple_to_list(Tuple)).

splitonhead([],_,_Map) ->
  [];
splitonhead([Hd|Hds],Traces,Map) ->
  [{Hd,[Tls || [E|Tls]<-Traces, st(Map(E))==st(Hd)]} | splitonhead(Hds,Traces,Map)].            
  
ifadd(List,Set,Elem) when Set=/=[]->
  [Elem|List];
ifadd(List,_,_Elem) ->
  List.


