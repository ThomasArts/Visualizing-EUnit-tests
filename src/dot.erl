%%%-------------------------------------------------------------------
%%% File    : dot.erl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Translates an automata to language DOT
%%%
%%% Created : 09 Feb 2010
%%%-------------------------------------------------------------------

-module(dot).

%% API
-export([visualize/1]).

visualize({fa,States,_Tokens,InitState,Transitions,FailingStates}) ->
    Dot = lists:flatten(["digraph G {\n",
			 translate_initstate(InitState),
			 lists:map(fun translate_failingstate/1, FailingStates),
			 lists:map(fun translate_normalstate/1,
				   (States -- [InitState]) -- FailingStates),
			 lists:map(fun translate_transition/1,
				   mix_transitions(Transitions)),
			 "}\n"]),
    file:write_file("qsm-output.dot", list_to_binary(Dot)),
    os:cmd("dot -Tps qsm-output.dot -o dot-output.ps"),
    file:delete("qsm-output.dot"),
    os:cmd("evince dot-output.ps"),
    file:delete("dot-output.ps").

%% Internal

removeLast([_|[]]) -> [];
removeLast([H|T]) -> [H|removeLast(T)].


mix_transitions(Transitions) ->
    lists:map(fun ({A,B}) ->
		      {A, removeLast(lists:flatten(
    [io_lib:format("~p~n", [C]) || {D, C, E} <- Transitions, D =:= A, E =:= B]
				      )),B}
	      end,
	      lists:usort([{A, B} || {A,_,B} <- Transitions])).

translate_initstate(InitState) ->
    lists:flatten(io_lib:format("  ~p [shape=doublecircle];~n", [InitState])).
translate_normalstate(NormalState) ->
    lists:flatten(io_lib:format("  ~p [shape=circle];~n", [NormalState])).
translate_failingstate(FailingState) ->
    lists:flatten(io_lib:format("  ~p [shape=polygon,sides=8,peripheries=3,color=red,style=filled]~n", [FailingState])).
translate_transition({Ori, Label, Dest}) ->
    lists:flatten(io_lib:format("  ~p -> ~p [label=~p];~n",
		  [Ori, Dest, Label])).
