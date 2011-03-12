%%%-------------------------------------------------------------------
%%% File    : apta.erl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Implements the functions related to APTA's.
%%%
%%% Created : 26 Oct 2010
%%%-------------------------------------------------------------------
-module(bluefringe_apta).

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
    %% ESp = positiveFromNegative(Sm)++Sp,
    ExpAgd = recursivelyExpandNodes(Agd, [{0, Sm, Sp}]),
    #fa{st = generateList(ExpAgd#agd.lastSt),
     alph = ExpAgd#agd.alph, iSt = 0, tr = ExpAgd#agd.tr,
     fSt = ExpAgd#agd.rSt}.


%%====================================================================
%% Internal functions
%%====================================================================

%% positiveFromNegative(List) -> lists:map(fun removeLast/1, List).

%% removeLast([]) -> [];
%% removeLast([_]) -> [];
%% removeLast([Head|Tail]) when Tail =/= [] -> [Head|removeLast(Tail)].

generateList(N) -> generateList(N, []).
generateList(0, List) -> [0 | List];
generateList(N, List) when (N > 0) -> generateList(N - 1, [N | List]).

%% cleanTransitions removes the first item from the 4-tuples in the list
cleanTransitions(List) ->
    Remover = fun ({_Transition, State, Failure, Acceptance}) ->
    {State, Failure, Acceptance} end,
    lists:map(Remover, List).

%% addToFail adds a transition to the list assuming it was a negative one
addToFail([Transition|Tail], {Transition, State, Failure, Acceptance}) ->
    {Transition, State, [Tail|Failure], Acceptance}.

%% addToAccept adds a transition to the list assuming it was a positive one
addToAccept([Transition|Tail], {Transition, State, Failure, Acceptance}) ->
    {Transition, State, Failure, [Tail|Acceptance]}.

addFailingState(StateNum, #agd{rSt = RSt} = Agd) ->
    Agd#agd{rSt = sets:add_element(StateNum, RSt)}.
addSymbol(Symbol, #agd{alph = Alph} = Agd) ->
    Agd#agd{alph = sets:add_element(Symbol, Alph)}.
addTransition(Transition, #agd{tr = Tr} = Agd) ->
    Agd#agd{tr = [Transition|Tr]}.

%% expandTrace expands a route and registers the transition
% expandTrace({Type, AddFun, Trace, StateNum}=TraceInfo, {Agd, Buffer}) -> {Agd, Buffer}
expandTrace({pos, _, [], _}, {Agd, Buffer}) -> {Agd, Buffer};
expandTrace({neg, _, [], StateNum}, {Agd, Buffer}) ->
    {addFailingState(StateNum, Agd), Buffer};
expandTrace({_, Add, [H|_] = Trace, StateNum}, {Agd, Buffer}) ->
    case searchMatchInBuffer(H, Buffer) of
	no_match ->
	    NewState = Agd#agd.lastSt + 1,
	    NewAgd = addSymbol(H, addTransition({StateNum, H, NewState},
						Agd#agd{lastSt = NewState})),
	    NewBufferEntry = {H, NewState, [], []},
	    {NewAgd, [Add(Trace, NewBufferEntry)|Buffer]};
	{Match, RestOfBuffer} -> {Agd, [Add(Trace, Match)|RestOfBuffer]}
    end.

searchMatchInBuffer(H, B) ->
    searchMatchInBuffer([], H, B).
searchMatchInBuffer(_, _, []) -> no_match;
searchMatchInBuffer(Buffer, Head,
		    [{Head, _, _, _} = Match|Tail]) ->
    {Match, Buffer++Tail};
searchMatchInBuffer(Buffer, H, [NoMatch|Tail]) ->
    searchMatchInBuffer([NoMatch|Buffer], H, Tail).

checkConflicts(Failure, Acceptance) ->
    Ends = lists:filter(fun ([]) -> true; (_) -> false end, Failure),
    case {Ends, Acceptance} of
	{[], _} -> ok;
	{_, []} when length(Failure) =:= length(Ends) -> ok;
	_ -> throw({error, conflictingTraces})
    end.

%% expandOneNode expands one single node and returns a list of
% new states (and the new agd)
expandOneNode(Agd, {_, Failure, Acceptance} = State) ->
    checkConflicts(Failure, Acceptance),
    expandOneNode1({Agd, []}, State).
expandOneNode1({Agd, Buffer}, {_, [], []}) ->
    {Agd, cleanTransitions(Buffer)};
expandOneNode1(AgdAndBuffer, State) ->
    {TraceInfo, NewState} = extractTrace(State),
    expandOneNode1(expandTrace(TraceInfo, AgdAndBuffer), NewState).

extractTrace({StateNum, [FailingTrace|Rest], []}) ->
    {{neg, fun addToFail/2, FailingTrace, StateNum}, {StateNum, Rest, []}};
extractTrace({StateNum, FailingTraces, [AcceptanceTrace|Rest]}) ->
    {{pos, fun addToAccept/2, AcceptanceTrace, StateNum},
     {StateNum, FailingTraces, Rest}}.

%% expandNodes aplies expandOneNode to each Node passing Agd
expandNodes(Agd, Nodes) -> expandNodes(Agd, [], Nodes).
expandNodes(Agd, ExpandedNodes, []) -> {Agd, ExpandedNodes};
expandNodes(Agd, ExpandedNodes, [NodeToExpand|OtherNodes]) ->
    {NewAgd, NewNodes} = expandOneNode(Agd, NodeToExpand),
    expandNodes(NewAgd, NewNodes++ExpandedNodes, OtherNodes).

recursivelyExpandNodes(Agd, Nodes) ->
    recursivelyExpandNodes({transformToSets(Agd), Nodes}).
recursivelyExpandNodes({Agd, []}) -> transformFromSets(Agd);
recursivelyExpandNodes({Agd, List}) -> New = expandNodes(Agd, List),
				       recursivelyExpandNodes(New).

transformToSets(#agd{rSt = RSt, alph = Alph} = Agd) ->
    Agd#agd{rSt = sets:from_list(RSt), alph = sets:from_list(Alph)}.

transformFromSets(#agd{rSt = RSt, alph = Alph} = Agd) ->
    Agd#agd{rSt = sets:to_list(RSt), alph = sets:to_list(Alph)}.


