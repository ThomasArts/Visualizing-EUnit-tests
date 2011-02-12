%%% File    : qsm_wrapper.erl
%%% Author  : Hans Svensson <>
%%% Description : Erlang wrapper for QSMTool (java)
%%% Created : 23 Nov 2010 by Hans Svensson

-module(qsm_wrapper).
-compile(export_all).

-define(STATECHUM,"/home/palas/erasmus/statechum.jar").

 
run(StateChum,Positive,Negative) ->
  %% io:format("POS: ~p\nNEG: ~p\n",[Positive,Negative]),
  File = write_qsm_file(Positive,Negative),
  _Res = os:cmd("java -jar " ++ StateChum ++ " " ++ File),
  Graph = parse_qsm_res().

run(Pos,Neg) ->
  run(?STATECHUM,Pos,Neg).

parse_qsm_res() ->
  ResFile = "./temp/dotOutput.dot",
  {ok,Data} = file:read_file(ResFile),
  %% io:format("Graph: ~s\n",[binary_to_list(Data)]),
  Lines = string:tokens(binary_to_list(Data),"\n"),
  States = [lists:takewhile(fun(C) -> C /= 91 end,Line) %% 91 == $[
            || Line <- Lines,
               nomatch /= re:run(Line,"^[0-9]+\\[")],
  Trans  = [parse_trans(Line)
            || Line <- Lines,
               nomatch /= re:run(Line,"->")],
  %% io:format("States: ~p\nTrans: ~p\n", [States,Trans]),
% Imposible to know the initial state...
  {States, hd(States), Trans}.
%%  Ini = case Trans of
%%          [{_, _, _}|_] -> usort([Ori || {Ori, _, _} <- Trans]) --
%%	                   [Dest || {_, ;
%%         _ -> hd(States)
%%        end,
%%  {States,Ini,Trans}.

parse_trans(Line) ->
  From = lists:takewhile(fun(C) -> C /= $\ end,Line),
  Line1 = tl(tl(lists:dropwhile(fun(C) -> C /= $> end,Line))),
  To = lists:takewhile(fun(C) -> C /= 91 end,Line1),
  LabelStr = lists:takewhile(fun(C) -> C /= $\" end,tl(lists:dropwhile(fun(C) -> C /= $\" end,Line))),
  Labels = [ list_to_atom(S) || S <- string:tokens(LabelStr,"\\n")],
  {From,Labels,To}.

write_qsm_file(Positive,Negative) ->
  TmpFile = "/tmp/qsm_" ++ integer_to_list(erlang:phash2(make_ref())),
  ConfData = "passive\ndotoutput\n",
  PosData = data_case("+ ", Positive),
  NegData = data_case("- ", Negative),
  %% io:format("Data:\n~s\n",[ConfData ++ PosData ++ NegData]),
  file:write_file(TmpFile,ConfData ++ PosData ++ NegData),
  TmpFile.

data_case(_Pre,[]) ->
  "";
data_case(Pre,[Case | Cases]) ->
  Pre ++ lists:flatten(lists:map(fun(Label) -> lists:flatten(io_lib:format("~p",[Label])) ++ " " end,Case))
    ++ "\n" ++ data_case(Pre,Cases).



