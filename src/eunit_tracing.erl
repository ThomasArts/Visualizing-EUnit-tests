-module(eunit_tracing).

-export([t/0,t/1]).

-export([test_start/0, test_end/0, test_negative/0, test_negative/1]).

-export([map_tuple/2, test_wrap/1, test__wrap/1, negative_wrap/1]).

-include("../include/tracing.hrl").

%%
%% Top-level function to initiate tracing.
%%

%% Tracing function which traces calls to functions in the argument Mod.erl
%% plus the "skip" functions defined here and used to punctuate the trace.

%% If you are tracing already, need to integrate this with your tracing
%% functionality.


t(Mod)
  when is_atom(Mod) ->
    code:load_file(Mod),
    code:load_file(addTestSuffix(Mod)),
    erlang:trace(all, true, [call]),
    erlang:trace_pattern({?tracing, open, '_'}, true, [local]),
    erlang:trace_pattern({?tracing, close, '_'}, true, [local]),    
    erlang:trace_pattern({?tracing, test_negative, '_'}, true, [local]),    
    erlang:trace_pattern({Mod, '_', '_'}, true, [global]).

%% Included for (internal) testing purposes.

t() ->
    t(frequency).

%% Library function: add _test to a module name.

addTestSuffix(Mod) ->
    list_to_atom(lists:concat([Mod,"_test"])).

%%
%% Functions to punctuate the trace.
%%

%% These are "skip" functions with no effect other than 
%% to be called and so to appear in the trace.

test_start() ->
    open(test).

test_end() ->
    close(test).

test_negative() ->
    ok.

test_negative(Test) ->
    Test.

open(_) ->
     ok.

open_(Atom) ->
    fun () -> open(Atom) end.

close(_) ->
     ok.

close_(Atom) ->
    fun (_) -> close(Atom) end.

%%
%% Wrapping up tests: i.e. wrapping in sufficiently 
%%

%% Map over a tuple (done by conversion to/from a list).

map_tuple(F,T) ->
    list_to_tuple(lists:map(F,tuple_to_list(T))).

%% Wrap a ..._test()
%% Can be positive or negative.

test_wrap(F) ->
    test_start(),
    F(),
    test_end().

%% Wrap a single component of a ..._test_()
%% Can be positive or negative.

test__wrap(F)
  when is_function(F) ->
    fun () ->
	    test_start(),
	    F(),
	    test_end()
    end;  
test__wrap(F)
  when is_tuple(F) ->
    case F of
	{setup,Setup,Tests} ->
	    {setup,
	     open_(setup),
	     close_(setup),
	     {setup,test__wrap(Setup),test__wrap(Tests)}};
	{setup,Setup,Teardown,Tests} ->
	    {setup,
	     open_(setup),
	     close_(setup),
	     {setup,test__wrap(Setup),teardown_wrap(Teardown),test__wrap(Tests)}};
	{inorder,Tests} ->
	    {setup,
	     open_(inorder),
	     close_(inorder),
	     {inorder, lists:map(fun test__wrap/1,Tests)}};
	{inparallel,Tests} ->
	    {setup,
	     open_(inparallel),
	     close_(inparallel),                            % not an error; do them in order so
	     {inorder, lists:map(fun test__wrap/1,Tests)}}; % as to separate traces.
	{inparallel,_,Tests} ->
	    {setup,
	     open_(inparallel),
	     close_(inparallel),                            % not an error; do them in order so
	     {inorder, lists:map(fun test__wrap/1,Tests)}}; % as to separate traces.
	{foreach,Setup,Teardown,Tests} ->
	    {setup,
	     open_(foreach),
	     close_(foreach),
	     {foreach,test__wrap(Setup),teardown_wrap(Teardown),test__wrap(Tests)}};
	_ ->    
	    map_tuple(fun test__wrap/1,F)
    end;
test__wrap(F)
  when is_list(F)->
    {setup,
     open_(list),
     close_(list),
     lists:map(fun test__wrap/1,F)};
test__wrap(F) ->
    F.

teardown_wrap(F) ->
    fun (R) ->
	    test_start(),
	    F(R),
	    test_end()
    end.	    

%% Mark a test as negative.
%% Used in the redefinition of _assertError etc.

negative_wrap(F)
  when is_function(F) ->
    fun () ->
	    F(),
	    test_negative()
    end;  
negative_wrap(F)
  when is_tuple(F) -> 
    map_tuple(fun negative_wrap/1,F);
negative_wrap(F) ->
    F.
