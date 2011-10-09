%%%-------------------------------------------------------------------
%%% File    : eunit_to_fsm.erl
%%% Author  : Pablo Lamela <lamela@student.chalmers.se>
%%% Description : Module to collect the main functions of the library
%%%
%%% Created : 11 Feb 2011 by Pablo Lamela
%%% Modified: 18 Feb 2011 by Thomas Arts
%%% Modified: 07 Mar 2011 by Simon Thompson
%%%-------------------------------------------------------------------
-module(eunit_to_fsm).
-compile(export_all).

%% API
-export([dynamic/2, dynamic/3, static/2, file/1, file/2, alternative_file/2, visualize/1]).

-define(negative, '-negative-').

% @spec (filename(),[compiler_option()]) -> {{[trace()],[trace()]},syntax_tree()}
% @doc Extract traces from EUnit file FileName_tests if it exists, otherwise assume the 
% tests to be provided in FileName. We trace all calls to functions exported in FileName.
% Compiler options passed as second argument.
% Optional third argument is list of API functions for the module.

dynamic(FileName,Options) ->
    dynamic(FileName,Options,none).

dynamic(FileName,Options,API) ->
    BaseName = list_to_atom(filename:basename(FileName,".erl")),
    {TestFile,File} = fix_source(FileName, fun eunit_macro_expander:dynamic_file/1),
    {ok,Module,Binary} = compile:file("/tmp/"++File,[binary|Options]),
    code:purge(BaseName),  % as otherwise repeated evaluations give error.
    code:delete(BaseName), % Added to purge code for module.erl
    {module,_} = code:load_binary(Module,File,Binary),
    Tree = teardowns:get_cleanup(TestFile),
    case API of
	none ->
	     {trace_runner:start(Module),Tree};
	_ ->
	    {trace_runner:start(Module,API),Tree}
    end.

fix_source(FileName, Expander) ->
    TestFile = filename:rootname(FileName,".erl")++"_tests.erl",
    {File,Strings} = 
	case filelib:is_file(TestFile) of
	    true ->
		Expander(TestFile);
	    false ->
		case filelib:is_file(FileName) of
		    true ->
			Expander(FileName);
		    false ->
			exit({enoent,FileName})
		end
	end,
    ok = file:write_file("/tmp/"++File,Strings),
    {TestFile,File}.

% @spec (filename(),[compiler_option()]) -> {{[trace()],[trace()]},syntax_tree()}
% @doc Staticly interprets EUnit file FileName_tests if it exists, otherwise assume the 
% tests to be provided in FileName. The tests are not run to obtain the traces.
% Complier_option are ignored.
static(FileName,_Options) ->
    {TestFile,File} = fix_source(FileName, fun eunit_macro_expander:static_file/1),
    Tree = teardowns:get_cleanup(TestFile),
    {static_parser:parse_file("/tmp/"++File),Tree}.

% @spec (filename()) -> {[trace()],[trace()]}
% @equiv erun(FileName,fun({_,F,_}) -> F end)
file(Filename) -> 
  file(Filename, fun({_,F,_}) -> F end).

% @spec (filename(),abstraction()) -> {[trace()], [trace()]}
% @doc Interprets the EUnit file FileName and extracts traces
%   of the functions called in the tests. The pair of traces
% consists of first positive and second negative traces.
%   A abstraction function can be provided in the form:
%     fun({Module, Function, Arguments}) -> _ end.
% This abstraction is applied to each function occurring in the extracted
% traces.
file(Filename, Abstract) ->
  {Pos, Neg} = static(Filename, []),
  {[[Abstract(E) || E <- Trace] || Trace <- Pos],
   [[Abstract(E) || E <- Trace] || Trace <- Neg]}.

alternative_file(Filename, Abstract) ->
  {ok,Forms} = epp:parse_file(Filename,["."],[{'EUNIT_HRL',true}]),
  EunitForms = eunit_autoexport:parse_transform(Forms,[]),
  _X = [ io:format("~s\n",[erl_pp:form(T)]) || T<-EunitForms ],
  {SUT,SUTFunctions} = get_sut_functions(EunitForms),
  Tests = lists:flatten([ Funs || {attribute,_,export,Funs} <- EunitForms ]), 
  TestForms = [ {function,Line,Name,Arity,Clauses} || 
                {function,Line,Name,Arity,Clauses}<-Forms,
                lists:member({Name,Arity},Tests -- [{test,0}])],
                 %% EUnit parse transform always adds {test,0}
  FunForms = [ {function,Line,Name,Arity,Clauses} || 
               {function,Line,Name,Arity,Clauses}<-Forms,
               not lists:member({Name,Arity},Tests)],
  Traces = get_traces(TestForms,FunForms,SUT,SUTFunctions),
  {Pos,Neg} = 
    lists:foldl(fun({trace,Ls},{P,N}) ->
                    case lists:last(Ls) of
                      ?negative ->
                        {P,[butlast(Ls)|N]};
                      _ ->
                        {[Ls|P],N}
                    end
                end,{[],[]},Traces),       
  {[[Abstract(E) || E <- Trace] || Trace <- Pos],
   [[Abstract(E) || E <- Trace] || Trace <- Neg]}.


% @hidden
% @spec (forms()) -> { atom(),[ {atom(), arity()} ]}
% @doc Get the SUT functions from the forms and return 
% modules name of SUT with imported functions.
get_sut_functions(Forms) ->
  [Module] = [atom_to_list(Mod) || {attribute,_,module,Mod}<-Forms],
  case string:str(Module,"_test") of
    0 ->
      exit({module_name,Module});
    Suffix ->
      SUT = list_to_atom(string:substr(Module,1,Suffix-1)),
      {SUT,
        lists:flatten([ Funs || {attribute,_,import,{Mod,Funs}}<-Forms,
                               Mod == SUT])}
  end.

% @spec ([form()],[form()],atom(),[{atom(),arity()}]) -> [[signed_trace()]]
% @doc extract the traces
get_traces([],_,_,_) ->
  [];
get_traces([TestForm|TestForms],FunForms,SUT,SUTFunctions) ->
  all_traces(TestForm,FunForms,SUT,SUTFunctions)
  ++ get_traces(TestForms,FunForms,SUT,SUTFunctions).

%% @spec ([form()],...) -> [{trace,trace()}]
all_traces({function, _, _Name, _Arity, Clauses}, FunForms, SUT, SUTFunctions) ->
  lists:append([all_traces(Clause, FunForms, SUT, SUTFunctions) || Clause <- Clauses]);
all_traces({clause, _, _Pat, _Guards, Body}, FunForms, SUT, SUTFunctions) ->
  product([all_traces(Expr, FunForms, SUT, SUTFunctions) || Expr <- Body]);
all_traces({tuple, _, [{atom, _, ?negative}, Expr]}, FunForms, SUT, SUTFunctions) ->
  [append(Trace, {trace,[?negative]}) || Trace <- all_traces(Expr, FunForms, SUT, SUTFunctions)];
all_traces({tuple, _, [{atom, _, setup}, SetUp, _TearDown, Expr]}, FunForms, SUT, SUTFunctions) ->
  [append(Trace1, Trace2) || Trace1 <- all_traces(SetUp, FunForms, SUT, SUTFunctions),
                             Trace2 <- all_traces(Expr, FunForms, SUT, SUTFunctions)];
all_traces({call, _, Name, Args}, FunForms, SUT, SUTFunctions) ->
  ArgTraces = product([all_traces(Arg, FunForms, SUT, SUTFunctions) || Arg <- Args]),
  {M,F} = 
    case Name of 
      {remote,_,{atom,_,Mod},{atom,_,FN}} ->
        {Mod,FN};
      {atom,_,FName} ->
        case lists:member({FName,length(Args)},SUTFunctions) of
          true ->
            {SUT,FName};
          false ->
            {'_',FName}
        end
    end,
  case {M,F} of
    {SUT,_} ->
      [ append(Trace,{trace,[{SUT,F,Args}]}) || Trace<-ArgTraces];
    _ ->
      ArgTraces
  end;
all_traces({'fun',_,{clauses,Clauses}}, FunForms, SUT, SUTFunctions) ->
  lists:append([all_traces(Clause, FunForms, SUT, SUTFunctions) || Clause <- Clauses]);
all_traces({cons,_,Head,Tail},FunForms,SUT,SUTFunctions) ->
  [append(Trace1, Trace2) || Trace1 <- all_traces(Head, FunForms, SUT, SUTFunctions),
                             Trace2 <- all_traces(Tail, FunForms, SUT, SUTFunctions)];
all_traces({block,_,Exprs},FunForms,SUT,SUTFunctions) ->
   product([all_traces(Expr, FunForms, SUT, SUTFunctions) || Expr <- Exprs]);
all_traces(_Form,_,_,_) ->
  [].


append({trace,L1},{trace,L2}) ->
  {trace,append2(L1,L2)}.
   
append2([], Ls2) ->
    Ls2;
append2([?negative| _], _) ->
    [?negative];
append2([E| Es], Ls2) ->
    [E| append2(Es, Ls2)].
  
% @spec ([[{trace,trace()}]]) -> [{trace,trace()}]
product([]) ->
  [{trace,[]}];
product([[]|Lists]) ->
  product(Lists);
product([List|Lists]) ->
  [ append(Trace1,Trace2) || Trace1<-List,
                             Trace2<-product(Lists)].


% @spec (filename()) -> ok
% @doc Shows a finite state machine representation of the EUnit test
% tests defined in the given file.
visualize(FileName) ->
  {Pos,Neg} = file(FileName),
  bluefringe:dot({Pos,Neg}),
  {Pos,Neg}.

butlast(L) ->
  lists:reverse(tl(lists:reverse(L))).
