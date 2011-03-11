%%% File    : trace_runner_eqc.erl
%%% Author  : Thomas Arts <>
%%% Description : QuickCheck tests for trace_runner.erl
%%% Created : 10 Mar 2011 by Thomas Arts <>

-module(trace_runner_eqc).

-include_lib("eqc/include/eqc.hrl").
-include("../include/tracing.hrl").

-import(trace_runner,[is_initial/2, parse/1]).

-compile(export_all).

prop_is_initial() ->
  ?FORALL({L,Prefix},{list(int()),list(int())},
          conjunction(
            [{positive, is_initial(Prefix++L,Prefix)},
             {equiv, implies(is_initial(L,Prefix) andalso is_initial(Prefix,L), Prefix==L)}])).


prop_parse() ->
  Modes = [m1,m2,m3],
  MOC = [ {M,{?tracing,open,[M]},{?tracing,close,[M]}} || M<-Modes ],
  ?FORALL(Segments,segments(Modes),
          length(parse(Segments)) < 5).


%% generator for nested segments  
segments(Modes) ->
  ?SIZED(Size,segments(Modes,call,Size)).

segments(_Modes,Gen,0) ->
  list(Gen);
segments(Modes,Gen,Size) ->
  oneof([segments(Modes,Gen,0)]++
        [ ?LETSHRINK(SmallerSegments,list(segments(Modes,Gen,Size div 4)),
                     {M,SmallerSegments}) || M<-Modes]
       ).
  

struct() ->
  ?SIZED(Size,struct(call,Size)).

struct(Gen,0) ->
  test(Gen);
struct(Gen,Size) ->
  oneof([test(Gen),
         inparallel(Gen,Size-1)
        ]).

test(Gen) ->
  {test,list(Gen)}.

inparallel(Gen,Size) ->
  {inparallel,list(struct(Gen,Size div 4))}.

implies(P1,P2) ->
  (not P1) orelse P2.





