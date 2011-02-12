%%%-------------------------------------------------------------------
%%% File    : apta.erl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Implements the functions related to APTA's.
%%%
%%% Created : 26 Oct 2010
%%%-------------------------------------------------------------------
-module(apta).

%% API
-export([generateApta/1]).
-include("../include/apta.hrl").
-include("../include/automata.hrl").
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: generateApta(SetOfRules)
%% Description: Takes a set of rules S = {S+, S-} and returns the APTA
%%--------------------------------------------------------------------

generateApta({Sp, Sm}) ->
    Agd = #agd{},
    ESp = positiveFromNegative(Sm)++Sp,
    ExpAgd = recursivelyExpandNodes(Agd, [{0, Sm, ESp}]),
    #fa{st = generateList(ExpAgd#agd.lastSt),
     alph = ExpAgd#agd.alph, iSt = 0, tr = ExpAgd#agd.tr,
     fSt = ExpAgd#agd.rSt}.


%%====================================================================
%% Internal functions
%%====================================================================

positiveFromNegative(List) -> lists:map(fun removeLast/1, List).

removeLast([]) -> [];
removeLast([_]) -> [];
removeLast([Head|Tail]) when Tail =/= [] -> [Head|removeLast(Tail)].

generateList(N) -> generateList(N, []).
generateList(0, List) -> [0 | List];
generateList(N, List) when (N > 0) -> generateList(N - 1, [N | List]).

%% cleanTransitions removes the first item from the 4-tuples in the list
cleanTransitions(Lista) ->
    Remover = fun ({_Transition, State, Failure, Acceptance}) ->
    {State, Failure, Acceptance} end,
    lists:map(Remover, Lista).

%% addToFail adds a transition to the list assuming it was a negative one
addToFail({Transition, State, Failure, Acceptance}, [Transition|Tail]) ->
    {Transition, State, [Tail|Failure], Acceptance}.

%% addToAccept adds a transition to the list assuming it was a positive one
addToAccept({Transition, State, Failure, Acceptance}, [Transition|Tail]) ->
    {Transition, State, Failure, [Tail|Acceptance]}.

%% addPath expands a route and registers the transition
% the first parameter is {a, addToAccept} for positive traces
%                     or {f, addToFail} for negative traces
addPath({a, _}, Agd, List, [], _State) -> {result, Agd, List};
addPath({f, _}, Agd, _List, [], State) ->
    NewAgd = Agd#agd{rSt = sets:add_element(State, Agd#agd.rSt)},
    {crash, NewAgd};
addPath({S, F}, Agd, List, FirstPath, State) ->
    addPath({S, F}, Agd, [], List, FirstPath, State).
addPath({S, F}, Agd, ResList, [], [FPHead|_] = FP, State) ->
    NewState = Agd#agd.lastSt + 1,
    TmpAgd = Agd#agd{lastSt = NewState,
		     alph = sets:add_element(FPHead, Agd#agd.alph),
		     tr = [{State, FPHead, NewState}|Agd#agd.tr]},
    addPath({S, F}, TmpAgd, ResList, [{FPHead, NewState, [], []}], FP, State);
addPath({_, F}, Agd, ResList, [({FPHead, _NewSt, _, _} = HTail)|LTail],
	[FPHead|_] = FP, _State) ->
    NewList = LTail ++ [F(HTail, FP)|ResList],
    {result, Agd, NewList};
addPath({S, F}, Agd, ResList, [HTail|LTail], FP, State) ->
    addPath({S, F}, Agd, [HTail|ResList], LTail, FP, State).

addToAcceptExtra(Agd, ExpandedTransitions, FirstAcceptance, State) ->
    addPath({a, fun addToAccept/2}, Agd, ExpandedTransitions,
	    FirstAcceptance, State).
addToFailExtra(Agd, ExpandedTransitions, FirstFailure, State) ->
    addPath({f, fun addToFail/2}, Agd, ExpandedTransitions,
	    FirstFailure, State).

%% expandOneNode expands one single node and returns a list of
% new states (and the new agd)
expandOneNode(Agd, {State, Failure, Acceptance}) ->
    expandOneNode(Agd, [], {State, Failure, Acceptance}).
expandOneNode(Agd, ExpandedTransitions,
	      {State, Failure, [FirstAcceptance|TailAcceptance]}) ->
    {result, NewAgd, NewExpandedT} = addToAcceptExtra(Agd, ExpandedTransitions,
						      FirstAcceptance, State),
    expandOneNode(NewAgd, NewExpandedT, {State, Failure, TailAcceptance});
expandOneNode(Agd, ExpandedTransitions,
	      {State, [FirstFailure|TailFailure], []}) ->
    case addToFailExtra(Agd, ExpandedTransitions, FirstFailure, State) of
	{result, NewAgd, NewExpandedT} ->
	      expandOneNode(NewAgd, NewExpandedT, {State, TailFailure, []});
	{crash, NewAgd} -> {NewAgd, []}
    end;
expandOneNode(Agd, ExpandedTransitions, {_State, [], []}) ->
    {Agd, cleanTransitions(ExpandedTransitions)}.
    

%% expandNodes aplies expandOneNode to each Node passing Agd
expandNodes(Agd, Nodes) -> expandNodes(Agd, [], Nodes).
expandNodes(Agd, ExpandedNodes, []) -> {Agd, ExpandedNodes};
expandNodes(Agd, ExpandedNodes, [NodeToExpand|OtherNodes]) ->
    {NewAgd, NewNodes} = expandOneNode(Agd, NodeToExpand),
    expandNodes(NewAgd, NewNodes++ExpandedNodes, OtherNodes).

recursivelyExpandNodes(Agd, Nodes) ->
    recursivelyExpandNodes({Agd#agd{rSt = sets:from_list(Agd#agd.rSt),
				    alph = sets:from_list(Agd#agd.alph)}, Nodes}).
recursivelyExpandNodes({Agd, []}) -> Agd#agd{rSt = sets:to_list(Agd#agd.rSt),
					     alph = sets:to_list(Agd#agd.alph)};
recursivelyExpandNodes({Agd, List}) -> New = expandNodes(Agd, List),
				       recursivelyExpandNodes(New).


