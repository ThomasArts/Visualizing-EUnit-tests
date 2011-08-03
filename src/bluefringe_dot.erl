%%%-------------------------------------------------------------------
%%% File    : dot.erl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Translates an automata to language DOT
%%%
%%% Created : 09 Feb 2010
%%%-------------------------------------------------------------------

-module(bluefringe_dot).

%% API
-export([visualize/1, visualize_debug/1]).

-import(bluefringe_apta, [ksort/2]).

simplePP(X) -> io_lib:format("~p", [X]).
simplePP2(X) -> io_lib:format("~p", [X]).

visualize(Automata) -> visualize(Automata, fun simplePP/1, fun simplePP2/1).
visualize_debug(Automata) -> visualize_debug(Automata, fun simplePP/1, fun simplePP2/1).

visualize(Automata,PPAbstracted,PPNonAbstracted) -> visualize(Automata,PPAbstracted,PPNonAbstracted,"jpeg").

visualize_debug(Automata,PPAbstracted,PPNonAbstracted) -> visualize(Automata,PPAbstracted,PPNonAbstracted,"svg").

visualize({fa,States,_Tokens,InitState,Transitions,FailingStates},PPAbstracted,PPNonAbstracted,Type) ->
    Name = "eunit_fsm",
    Dot = lists:flatten(["digraph G {\n",
			 translate_initstate(InitState),
			 lists:map(fun translate_failingstate/1, FailingStates),
			 lists:map(fun translate_normalstate/1,
				   (States -- [InitState]) -- FailingStates),
			 lists:map(fun translate_transition/1,
				   mix_transitions(Transitions,PPAbstracted,PPNonAbstracted)),
			 "}\n"]),
    file:write_file(Name++".dot", list_to_binary(Dot)),
    oscmd("GRAPHVIZ","dot","-T"++Type++" "++Name++".dot -o"++Name++"."++Type),
    oscmd("VIEWER","open",Name++"."++Type).

oscmd(EnvVar,Default,Args) ->
    CmdName = case os:getenv(EnvVar) of
		  false ->
		      Default;
		  S ->
		      S
	      end,
    Cmd = CmdName ++ " " ++ Args,
    io:format("Running ~s (set ~s to change)~n",[Cmd,EnvVar]),
    io:format(os:cmd(Cmd)).

%% Internal

removeLast([_|[]]) -> [];
removeLast([H|T]) -> [H|removeLast(T)].

flatten_and_remove_last(List) -> {Abstracted, NoAbstracted} = lists:unzip(List),
								 {removeLast(lists:flatten(Abstracted)), removeLast(lists:flatten(NoAbstracted))}.

mix_transitions(Transitions,PPAbstracted,PPNonAbstracted) ->
	lists:map(fun ({A,B}) ->
					   {A, flatten_and_remove_last([{lists:flatten(io_lib:format("~s~n", [PPAbstracted(C)])),
							 lists:flatten(io_lib:format("~s: ~s~n", [PPAbstracted(C), PPNonAbstracted(EI)]))}
						 || {D, {C, EI}, E} <- Transitions, D =:= A, E =:= B]), B}
			  end,
			  lists:usort([{A, B} || {A,_,B} <- Transitions])).

translate_initstate(InitState) ->
    lists:flatten(io_lib:format("  ~p [shape=doublecircle];~n", [InitState])).
translate_normalstate(NormalState) ->
    lists:flatten(io_lib:format("  ~p [shape=circle];~n", [NormalState])).
translate_failingstate(FailingState) ->
    lists:flatten(io_lib:format("  ~p [shape=polygon,sides=8,peripheries=3,color=red,style=filled]~n", [FailingState])).
translate_transition({Ori, {Label, History}, Dest}) ->
    lists:flatten(io_lib:format("  ~p -> ~p [label=~p, edgetooltip=~p];~n",
		  [Ori, Dest, Label, History])).
