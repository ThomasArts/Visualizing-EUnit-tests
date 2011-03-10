%%% File    : trace_runner_eqc.erl
%%% Author  : Thomas Arts <>
%%% Description : QuickCheck tests for trace_runner.erl
%%% Created : 10 Mar 2011 by Thomas Arts <>

-module(trace_runner_eqc).

-include_lib("eqc/include/eqc.hrl").

-import(trace_runner,[is_initial/2]).

-compile(export_all).

prop_is_initial() ->
  ?FORALL({L1,L2,L3},{list(int()),list(int()),non_empty(list(int()))},
              conjunction(
                [{positive, is_initial(L1,L1++L2)},
                 {negative, implies(is_initial(L3,L1) andalso is_initial(L1,L3), L1==L3)}])).

implies(P1,P2) ->
  (not P1) orelse P2.





