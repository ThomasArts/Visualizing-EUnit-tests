%%%-------------------------------------------------------------------
%%% File    : eunit_tracing.erl
%%% Author  : Simon Thompson
%%% Description : Module to collect traces by running the tests against
%%% the SUT.
%%%
%%% Created : Feb 2011 by Simon Thompson, Thomas Arts and Pablo Lamela
%%% Modified: Aug 2011 by Simon Thompson
%%%-------------------------------------------------------------------

-module(eunit_tracing).

-export([t/1]). %% top-level function to initiate tracing of functions
                %% in the module that is its argument.

-export([test_start/1, test_end/0]).
-export([test_negative/1, test_negative/2, test_positive/1, test_positive/2, line/1]).
-export([map_tuple/2, test_wrap/2, test__wrap/2, negative_wrap/2, positive_wrap/2]).

-include("../include/visualize.hrl").

%%%-------------------------------------------------------------------
%%
%% Top-level function to initiate tracing.
%%
%%%-------------------------------------------------------------------

%% Tracing function which traces calls to functions in the argument Mod.erl
%% plus the functions defined here that are used to punctuate the trace.

%% If you are tracing already, you will need to integrate this
%% with your tracing functionality.

%% August 2011: added tracing of function returns from Mod, also added the
%% facility to record expected return value of positive tests, and to
%% record line number information in the traces.


t(Mod)
  when is_atom(Mod) ->
    code:purge(Mod),
    code:load_file(Mod),
    code:load_file(addTestSuffix(Mod)),
    erlang:trace(all,true,[call]),
    erlang:trace_pattern({?tracing, open, '_'}, true, [local]),
    erlang:trace_pattern({?tracing, close, '_'}, true, [local]),
    erlang:trace_pattern({?tracing, test_negative, 1}, true, [local]),
    erlang:trace_pattern({?tracing, test_positive, 1}, true, [local]),
    erlang:trace_pattern({?tracing, line, 1}, true, [local]),
    erlang:trace_pattern({Mod, '_', '_'}, true, [global]),
    erlang:trace_pattern({Mod,'_','_'},[{'_',[],[{return_trace}]}],[global]).

%% Auxiliary function: add _test to a module name.

addTestSuffix(Mod) ->
    list_to_atom(lists:concat([Mod,"_test"])).

%%%-------------------------------------------------------------------
%%
%% Functions to punctuate the trace.
%%
%%%-------------------------------------------------------------------

%% These are "skip" functions with no effect other than 
%% to be called and so to appear in the trace.

line(_LineNumber) ->
    ok.

test_start(LineNumber) ->
    open(test),
    line(LineNumber).

test_end() ->
    close(test).

test_negative(_Assert) ->
    ok.

test_negative(Assert,Test) ->
    test_negative(Assert), % Added so that can trace test_negative/1 rather than .../1,/2.
    Test.

test_positive(_Assert) ->
    ok.

test_positive(Assert,Test) ->
    test_positive(Assert), % Added so that can trace test_positive/1 rather than .../1,/2.
    Test.

open(_) ->
     ok.

open_(Atom) ->
    fun () -> open(Atom) end.

close(_) ->
     ok.

close_(Atom) ->
    fun (_) -> close(Atom) end.


%%%-------------------------------------------------------------------
%%
%% Wrapping up tests: surround with enough calls to the functions
%% above to be able to parse the resulting log of function calls.
%%
%%%-------------------------------------------------------------------

%% Auxiliary function: map over a tuple (done by conversion to/from a list).

map_tuple(F,T) ->
    list_to_tuple(lists:map(F,tuple_to_list(T))).

%% Wrap a ..._test()
%% Can be positive or negative.

test_wrap(F,LineNumber) ->
    test_start(LineNumber),
    F(),
    test_end().

%% Wrap a single component of a ..._test_()
%% Can be positive or negative.

test__wrap(F,LineNumber)
  when is_function(F) ->
    case element(2,erlang:fun_info(F,arity)) of
	0 ->
	    fun () ->
		    test_start(LineNumber),
		    Result = F(),
		    test_end(),
		    Result
		end;
	1 ->
	    fun (X) ->
		    test_start(LineNumber),
		    Result = F(X),
		    test_end(),
		    Result
		end;
	_ ->
	    error
    end;

test__wrap(F,LineNumber)
  when is_tuple(F) ->
    case F of
	{setup,Setup,Tests} ->
	    {setup,
	     open_(setup),
	     close_(setup),
	     {setup,setup_wrap(Setup,LineNumber),test__wrap(Tests,LineNumber)}};
	{setup,Setup,Teardown,Tests} ->
	    {setup,
	     open_(setup),
	     close_(setup),
	     {setup,setup_wrap(Setup,LineNumber),teardown_wrap(Teardown,LineNumber),test__wrap(Tests,LineNumber)}};
	{inorder,Tests} ->
	    {setup,
	     open_(inorder),
	     close_(inorder),
	     {inorder, lists:map(fun (X) -> test__wrap(X,LineNumber) end,Tests)}};
	{inparallel,Tests} ->
	    {setup,
	     open_(inparallel),
	     close_(inparallel),                            % not an error; do them in order so
	     {inorder, lists:map(fun (X) -> test__wrap(X,LineNumber) end,Tests)}}; % as to separate traces.
	{inparallel,_,Tests} ->
	    {setup,
	     open_(inparallel),
	     close_(inparallel),                            % not an error; do them in order so
	     {inorder, lists:map(fun (X) -> test__wrap(X,LineNumber) end,Tests)}}; % as to separate traces.
	{foreach,Setup,Teardown,Tests} ->
	    {setup,
	     open_(foreach),
	     close_(foreach),
	     {foreach,foreach_setup_wrap(Setup,LineNumber),foreach_teardown_wrap(Teardown,LineNumber),test_list__wrap(Tests,LineNumber)}};
	_ ->    
	    map_tuple(fun (X) -> test__wrap(X,LineNumber) end,F)
    end;

test__wrap(F,LineNumber) 
  when is_list(F)->
     {setup,
      open_(list),
      close_(list),
      lists:map(fun (X) -> test__wrap(X,LineNumber) end,F)};

test__wrap(F,_LineNumber) ->
    F.

test_list__wrap(F,_LineNumber) 
  when is_list(F) -> F.

%% Setup wrap
setup_wrap(F,LineNumber)
  when is_function(F) ->
    case element(2,erlang:fun_info(F,arity)) of
	0 ->
	    fun () ->
		    test_start(LineNumber),
		    Result = F(),
		    test_end(),
		    Result
		end;
	1 ->
	    fun (X) ->
		    test_start(LineNumber),
		    Result = F(X),
		    test_end(),
		    Result
		end;
	_ ->
	    error
    end.



%% Wrapping the teardown part of an EUnit setup.

teardown_wrap(F,LineNumber) ->
    fun (R) ->
	    test_start(LineNumber),
	    Result = F(R),
	    test_end(),
	    Result
    end.	    

foreach_setup_wrap(F,LineNumber)
  when is_function(F) ->
    case element(2,erlang:fun_info(F,arity)) of
	0 ->
	    fun () ->
		    test_start(LineNumber),
		    Result = F(),
		    %test_end(),
		    Result
		end;
	1 ->
	    fun (X) ->
		    test_start(LineNumber),
		    Result = F(X),
		    %test_end(),
		    Result
		end;
	_ ->
	    error
    end.



%% Wrapping the teardown part of an EUnit setup.

foreach_teardown_wrap(F,_LineNumber) ->
    fun (R) ->
	    %test_start(LineNumber),
	    Result = F(R),
	    test_end(),
	    Result
    end.	    



%%%-------------------------------------------------------------------
%%
%% Marking tests as positive or negative, carrying information
%% about expected results.
%%
%%%-------------------------------------------------------------------


%% Mark a test as negative.
%% Used in the redefinition of _assertError etc.

negative_wrap(Info,F)
  when is_function(F) ->
    fun () ->
	    F(),
	    test_negative(Info)
    end;

negative_wrap(Info,F)
  when is_tuple(F) -> 
    map_tuple(fun(X) -> negative_wrap(Info,X) end,F);

negative_wrap(_Info,F) ->
    F.

%% Mark a test as positive.
%% Used in the redefinition of _assertEqual etc.


positive_wrap(Info,F)
  when is_function(F) ->
    fun () ->
	    F(),
	    test_positive(Info)
    end;

positive_wrap(Info,F)
  when is_tuple(F) -> 
    map_tuple(fun(X) -> positive_wrap(Info,X) end,F);

positive_wrap(Info,F)
  when is_list(F) -> 
    lists:map(fun(X) -> positive_wrap(Info,X) end,F);

positive_wrap(_Info,F)
  when is_atom(F) -> 
    F;

positive_wrap(_Assert,F) ->
    F.

