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
