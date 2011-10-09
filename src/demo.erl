%%% File    : demo.erl
%%% Author  : Thomas Arts <>
%%% Description : 
%%% Created : 14 May 2011 by Thomas Arts <>
%%% Modified : Oct 2011 by Simon Thompson (small mods to demo)
%%% Modified : Oct 2011 by Simon Thompson (added gf/0 function)

-module(demo).

-export([frequency/0,gf/0]).

gf() ->
  % first run the tests to get the traces
  {{Pos,Neg},CleanupTree} = eunit_to_fsm:dynamic("/Users/simonthompson/protest/Work/Visualizing-EUnit-tests/Gianfranco/src/wn_resource_layer.erl",
						 [{i,"/Users/simonthompson/protest/Work/Visualizing-EUnit-tests/Gianfranco/ebin"}],
						[start_link,register,list_resources,stop,deregister]),

  % second create an automata
  Automata = bluefringe:qsm({Pos,Neg}),

  % third transform automata into QuickCheck template
  String = bluefringe_fsm:eqc_fsm(Automata,wn_resource_layer,CleanupTree),
  ok = file:write_file("wn_resource_layer_eqc.erl",String),

  % Compile the QuickCheck code
  code:purge(wn_resource_layer_eqc),
  compile:file("wn_resource_layer_eqc.erl"),
  code:load_file(wn_resource_layer_eqc),
  eqc_fsm:visualize(wn_resource_layer_eqc).

frequency() ->
  % first run the tests to get the traces
  {{Pos,Neg},CleanupTree} = eunit_to_fsm:dynamic("../examples/frequency.erl",
                                   []),

  % second create an automata
  Automata = bluefringe:qsm({Pos,Neg}),

  % third transform automata into QuickCheck template
  String = bluefringe_fsm:eqc_fsm(Automata,frequency,CleanupTree),
  ok = file:write_file("frequency_eqc.erl",String),

  % Compile the QuickCheck code
  code:purge(frequency_eqc),
  compile:file("frequency_eqc.erl"),
  code:load_file(frequency_eqc),
  eqc_fsm:visualize(frequency_eqc).
