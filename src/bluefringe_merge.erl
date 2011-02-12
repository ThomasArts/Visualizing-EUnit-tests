%%%-------------------------------------------------------------------
%%% File    : merge.erl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Implements functions to merge states.
%%%
%%% Created : 7 Nov 2010
%%%-------------------------------------------------------------------
-module(bluefringe_merge).
-include("../include/automata.hrl").

%% API
-export([merge/3, number_of_merges/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: merge(Automata, OptimizedExtraInfo, State1, State2)
%% Description: Merges states in both automata and optimized extra
%%   info. Doesn't remove the eliminated states from the lists.
%%--------------------------------------------------------------------

merge(Automata, St1, St2) ->
    {Result, _} = merge_list(Automata, [{St1, St2}], 1),
    Result.

number_of_merges(Automata, St1, St2) ->
    case (catch merge_list(Automata, [{St1, St2}], 1)) of
        {_, Number} -> Number;
        incompatible -> -1
    end.

%%====================================================================
%% Internal functions
%%====================================================================

% Merges the list of pairs and the new pairs needed to make it
% deterministic. Updates OptimizedExtraInfo.
merge_list(Automata, [{St1, St2}|Tail], Number) ->
    {NewAutomata, NewMerges} = merge_one(Automata, St1, St2),
    merge_list(NewAutomata, NewMerges++Tail, Number + 1);
merge_list(Automata, [], Number) -> {Automata, Number}.

del_from_list(List, Element) ->
    lists:filter(fun (X) -> X =/= Element end, List).

is_in_list(List, Element) ->
    lists:any(fun (X) -> X =:= Element end, List).

% Merges one pair updating EI. Returns the list of new pairs needed
% to make it deterministic (return is not a valid, will be valid after
% the list of new pairs to merge is merged...)
merge_one(Automata, St1, St2) ->
    case {is_in_list(Automata#fa.st, St1), is_in_list(Automata#fa.st, St2)} of
	{true, true} ->
	    checkIfIncompatible(Automata#fa.fSt, St1, St2),
	    {NewTr, NewMerges} = merge_on_trList(Automata#fa.tr, St1, St2),
	    {Automata#fa{tr = NewTr,
			 st = del_from_list(Automata#fa.st, St2),
			 fSt = del_from_list(Automata#fa.fSt, St2)},
	     NewMerges};
	_ -> {Automata, []}
    end.

checkIfIncompatible(List, St1, St2) ->
    case lists:subtract([St1, St2], List) of
	[_] -> throw(incompatible);
	_ -> ok
    end.


% Merges one pair not updating EI. Returns the list of new pairs needed
% to make it deterministic
merge_on_trList(Tr, St1, St2) ->
    merge_on_trList([], Tr, St1, St2, [], []).
merge_on_trList(DoneL, [], _St1, _St2, Final, NewMerges) ->
    {Final++DoneL, NewMerges};
merge_on_trList(DoneL, [{Ori, Tra, Dest}|Tail], St1, St2, Final, NewMerges)
    when ((Ori == St1) or (Ori == St2) or (Dest == St1) or (Dest == St2)) ->
    {NewFinal, NewNewMerge} = merge_one_tr(St1, St2, {Ori, Tra, Dest}, Final),
    merge_on_trList(DoneL, Tail, St1, St2, NewFinal,
		    lists:merge(NewNewMerge, NewMerges));
merge_on_trList(DoneL, [{_, _, _} = Head|Tail], St1, St2, Final, NewMerges) ->
    merge_on_trList([Head|DoneL], Tail, St1, St2, Final, NewMerges).

replace(Or, De, A) ->
    case A of
	Or -> De;
	Other -> Other
    end.

replace_3t(Or, De, {A, B, C}) ->
    A2 = replace(Or, De, A),
    C2 = replace(Or, De, C),
    {A2, B, C2}.

merge_one_tr(St1, St2, {_, _, _} = Tupla, Final) ->
    merge_one_tr(replace_3t(St2, St1, Tupla), Final, []).
merge_one_tr({_, _, _} = Tupla, [], DoneF) ->
    {[Tupla | DoneF], []};
merge_one_tr({Ori, Tra, Dest} = Tupla, [{Ori, Tra, Dest}|Tail], DoneF) ->
    {[Tupla|Tail]++DoneF, []};
merge_one_tr({Ori, Tra, Dest1} = T1, [{Ori, Tra, Dest2}|Tail], DoneF) when Dest1 < Dest2 ->
    {[T1|(Tail++DoneF)], [{Dest1,Dest2}]};
merge_one_tr({Ori, Tra, Dest1}, [{Ori, Tra, Dest2} = T2|Tail], DoneF) when Dest2 < Dest1 ->
    {[T2|(Tail++DoneF)], [{Dest2,Dest1}]};
merge_one_tr({_, _, _} = Tupla1, [Tupla2|Tail], DoneF) ->
    merge_one_tr(Tupla1, Tail, [Tupla2|DoneF]).

