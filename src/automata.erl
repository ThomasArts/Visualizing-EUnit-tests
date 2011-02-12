%%%-------------------------------------------------------------------
%%% File    : automata.erl
%%% Author  : Pablo Lamela Seijas <lamela@student.chalmers.se>
%%% Description : Includes the definition and functions to work
%%%               with finite automatas (fa).
%%% Created : 26 Oct 2010
%%%-------------------------------------------------------------------
-module(automata).
-include("../include/automata.hrl").
%% API
-export([automataToTuple/1, tupleToAutomata/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: tupleToAutomata(Tuple)
%% Description: Takes a tuple in the form {Q,Q0,F,[],Delta} and
%%      returns an automata.
%%--------------------------------------------------------------------

tupleToAutomata({Q,Q0,F,Events,Delta}) ->
    #fa{st = Q, alph = Events, iSt = Q0, tr = Delta, fSt = F}.

%%--------------------------------------------------------------------
%% Function: automataToTuple(Automata)
%% Description: Takes an automata and returns a tuple in the
%%      form {Q,Q0,F,[],Delta}.
%%--------------------------------------------------------------------

automataToTuple(FA) ->
    {FA#fa.st, FA#fa.iSt, FA#fa.fSt, FA#fa.alph, FA#fa.tr}.



%%====================================================================
%% Internal functions
%%====================================================================

