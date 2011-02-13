%%%-------------------------------------------------------------------
%%% File    : eunit_to_fsm.erl
%%% Author  : Pablo Lamela <lamela@student.chalmers.se>
%%% Description : Module to collect the main functions of the library
%%%
%%% Created : 11 Feb 2011 by Pablo Lamela
%%%-------------------------------------------------------------------
-module(eunit_to_fsm).

%% API
-export([erun/1,erun/2, visualize/1]).

% @spec (filename()) -> {[trace()],[trace()]}
% @equiv erun(FileName,fun({_,F,_}) -> F end)
erun(Filename) -> 
  erun(Filename, fun({_,F,_}) -> F end).

% @spec (filename(),abstraction()) -> {[trace()], [trace()]}
% @doc Interprets the EUnit file FileName and extracts traces
%   of the functions called in the tests. The pair of traces
% consists of first positive and second negative traces.
%   A abstraction function can be provided in the form:
%     fun({Module, Function, Arguments}) -> _ end.
% This abstraction is applied to each function occurring in the extracted
% traces.
erun(Filename, Abstract) ->
  {Pos, Neg} = static_parser:parse_file(Filename),
  {[[Abstract(E) || E <- Trace] || Trace <- Pos],
   [[Abstract(E) || E <- Trace] || Trace <- Neg]}.


% @spec (filename()) -> ok
% @doc Shows a finite state machine representation of the EUnit test
% tests defined in the given file.
visualize(FileName) ->
  {Pos,Neg} = erun(FileName),
  bluefringe:dot(Pos,Neg).
