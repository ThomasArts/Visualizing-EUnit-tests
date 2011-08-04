%%% File    : trace_runner_eqc.erl
%%% Author  : Thomas Arts <>
%%% Description : QuickCheck tests for trace_runner.erl
%%% Created : 10 Mar 2011 by Thomas Arts <>

-module(trace_runner_eqc).

-include_lib("eqc/include/eqc.hrl").
-include("../include/visualize.hrl").

-import(trace_runner,[is_initial/2, parse/1]).

-compile(export_all).

prop_is_initial() ->
  ?FORALL({L,Prefix},{list(int()),list(int())},
          conjunction(
            [{positive, is_initial(Prefix++L,Prefix)},
             {equiv, implies(is_initial(L,Prefix) andalso is_initial(Prefix,L), Prefix==L)}])).


prop_parse() ->
  ?FORALL(Struct,struct({m,f,[a1]}),
          parse(pp([Struct])) == [Struct]).


prop_flatten_struct() -> 
  ?FORALL(S,struct(call),
          begin
            trace_runner:flatten_struct([S]),
            true
          end).

pp([]) ->
  [];
pp([{Mode,List}|T]) ->
  [ {?tracing,open,[Mode]}] ++ pp(List) ++ [{?tracing,close,[Mode]}] ++ pp(T);
pp(CallList) ->
  CallList.

struct(Gen) ->
  ?SIZED(Size,struct(Gen,Size)).

struct(Gen,0) ->
  test(Gen);
struct(Gen,Size) ->
  oneof([test(Gen),
         mode(inparallel,Gen,Size div 4),
         mode(inorder,Gen,Size div 4),
         mode(setup,Gen, Size div 4)
        ]).

test(Gen) ->
  {test,list(Gen)}.

mode(Tag,Gen,Size) ->
  {Tag,list(struct(Gen,Size))}.


implies(P1,P2) ->
  (not P1) orelse P2.





