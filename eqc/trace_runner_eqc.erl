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
  Open = {?tracing,open,[]},
  Close = {?tracing,close,[]},
  ?FORALL(Segments,segments(Open,Close),
          length(parse([Open]++Segments++[Close])) < 5).


%% generator for nested segments  
segments(Open,Close) ->
  ?SIZED(Size,segments(Open,Close,int(),Size)).

segments(_Open,_Close,Gen,0) ->
  list(Gen);
segments(Open,Close,Gen,Size) ->
  SmallerSegment = segments(Open,Close,Gen,Size div 3),
  ?LETSHRINK({S1,S2,S3}, {SmallerSegment,SmallerSegment,SmallerSegment},
             oneof([segments(Open,Close,Gen,0),
                    S1 ++ [Open] ++ S2 ++ [Close] ++ S3
                   ])).
  

implies(P1,P2) ->
  (not P1) orelse P2.





