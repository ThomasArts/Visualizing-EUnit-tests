%%%-------------------------------------------------------------------
%%% File    : visualize.hrl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Macro and record definitions for visualization of
%%% EUnit tests by state machines.
%%%
%%% Created : 26 Oct 2010
%%% Modified: 2 August 2011 by Simon Thompson 
%%%-------------------------------------------------------------------

%% st = states, alph = alphabet, iSt = initialState, tr = transitions
%% fSt = finalStates <- The final states will be the ones that crash
%% The rest would be acceptance states. But is quicker if we store
%% them this way.
-record(fa, {st, alph = [], iSt, tr = [], fSt = []}).

%% Defines the module in which the tracing functions live.
-define(tracing,'eunit_tracing').

%% Defines a record for tracing purposes: includes all the information
%% about a function call in a test: hence a "trace item" or titem.

-record(titem, {id,     % id of test giving rise to this item: line number
		mod,    % module of function called
		func,   % function called
		args,   % arguments of function call
		pn,     % polarity: pos or neg
		pat,    % pattern specifying expected result of function call
		res}).  % actual result of function call

