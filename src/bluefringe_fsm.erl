%%% File    : bluefringe_fsm.erl
%%% Author  : Thomas Arts <thomas.arts@quviq.com>
%%% Description : Transform a bluefringe automata into a template for a QuickCheck state machine
%%% Created :  9 May 2011 by Thomas Arts

-module(bluefringe_fsm).

-export([eqc_fsm/2,eqc_fsm/3,pp_eunit/2]).
-include("../include/visualize.hrl").


% @spec (titem(),atom()) -> syntaxTree()
% Writes a Bluefringe automata to file as a QuickCheck state machine template
eqc_fsm(Automata,Module) ->
  eqc_fsm(Automata,Module,undefined).

eqc_fsm(Automata,Module,CleanupTree) ->
  SyntaxTree = 
    to_tree(Automata,Module,CleanupTree),
  lists:flatten([io_lib:format("~s\n\n",[erl_prettypr:format(T)]) || T<-SyntaxTree]).

to_tree(Automata,Module,CleanupTree) ->
  %% introduce generators for transitions with different arguments
  Automata_1 = 
    rename_states(Automata),
  header(Module) ++
    generators(Automata_1) ++
    trailer(Module,arities(Automata#fa.alph)) ++
    cleanup(Module,CleanupTree).

header(Module) ->
  [erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(lists:concat([Module,"_eqc"]))]),
   mkinclude("eqc/include/eqc.hrl"),
   mkinclude("eqc/include/eqc_fsm.hrl"),
   erl_syntax:attribute(erl_syntax:atom(compile),[erl_syntax:atom(export_all)])
  ].

% precondition, next_state_data and postcondition
trailer(Module,Calls) ->
  % preconditions
  [ erl_syntax:function(
      erl_syntax:atom(precondition),
      [ erl_syntax:clause([erl_syntax:variable("_From"),erl_syntax:variable("_To"),erl_syntax:variable("_S"),
                           erl_syntax:tuple([erl_syntax:atom(call),
                                            erl_syntax:variable("_"),
                                            erl_syntax:abstract(Fun),
                                            erl_syntax:list([erl_syntax:variable("_") || _<-lists:seq(1,Args)])])],[],
                          [erl_syntax:atom(true) ]) ||
        {Mod,Fun,Args} <- Calls ])] ++
  % initial_state_data
  [ erl_syntax:function(
      erl_syntax:atom(initial_state_data),
      [ erl_syntax:clause([],[],
                          [erl_syntax:list([]) ])])]++
  % next_state_data
  [ erl_syntax:function(
      erl_syntax:atom(next_state_data),
      [ erl_syntax:clause([erl_syntax:variable("_From"),erl_syntax:variable("_To"),erl_syntax:variable("S"),
                           erl_syntax:variable("_V"),
                           erl_syntax:tuple([erl_syntax:atom(call),
                                            erl_syntax:variable("_"),
                                            erl_syntax:abstract(Fun),
                                            erl_syntax:list([erl_syntax:variable("_") || _<-lists:seq(1,Args)])])],[],
                          [erl_syntax:variable("S") ])  || {Mod,Fun,Args} <- Calls ])]++
  % postconditions
  [ erl_syntax:function(
      erl_syntax:atom(postcondition),
      [ erl_syntax:clause([erl_syntax:variable("_From"),erl_syntax:atom(state_error),erl_syntax:variable("_S"),
                           erl_syntax:variable("_Call"),erl_syntax:variable("R")],[],
                          [erl_syntax:case_expr(
                             erl_syntax:variable("R"),[
                                 erl_syntax:clause([erl_syntax:tuple(
                                                      [erl_syntax:atom('EXIT'),
                                                       erl_syntax:variable("_")])],[],[erl_syntax:atom(true)]),
                                 erl_syntax:clause([erl_syntax:variable("_")],[],[erl_syntax:atom(false)])])
                            ]) |
       [ erl_syntax:clause([erl_syntax:variable("_From"),erl_syntax:variable("_To"),erl_syntax:variable("_S"),
                           erl_syntax:tuple([erl_syntax:atom(call),
                                            erl_syntax:variable("_"),
                                            erl_syntax:abstract(Fun),
                                            erl_syntax:list([erl_syntax:variable("_") || _<-lists:seq(1,Args)])]),
                           erl_syntax:variable("R")],[],
                          [erl_syntax:case_expr(
                             erl_syntax:variable("R"),[
                                 erl_syntax:clause([erl_syntax:tuple(
                                                      [erl_syntax:atom('EXIT'),
                                                       erl_syntax:variable("_")])],[],[erl_syntax:atom(false)]),
                                 erl_syntax:clause([erl_syntax:variable("_")],[],[erl_syntax:atom(true)])])
                            ]) ||
        {Mod,Fun,Args} <- Calls ]])]++
  % property
  [erl_syntax:function(
      erl_syntax:atom(lists:concat(["prop_",Module])),
      [ erl_syntax:clause([],[],[
           erl_syntax:macro(erl_syntax:variable("FORALL"),[
             erl_syntax:variable("Cmds"),
             erl_syntax:application(erl_syntax:atom(commands),
                                    [erl_syntax:macro(erl_syntax:variable("MODULE"))]),
             erl_syntax:block_expr([
                erl_syntax:match_expr(erl_syntax:tuple([erl_syntax:variable("History"),
                                                        erl_syntax:variable("S"),
                                                        erl_syntax:variable("Res")]),
                                      erl_syntax:application(erl_syntax:atom(run_commands),
                                                            [erl_syntax:macro(erl_syntax:variable("MODULE")),
                                                             erl_syntax:variable("Cmds")])),
                erl_syntax:application(erl_syntax:atom(cleanup),[erl_syntax:variable("S")]),
                erl_syntax:macro(erl_syntax:variable("WHENFAIL"),
                                 [erl_syntax:application(erl_syntax:atom(bluefringe_fsm),erl_syntax:atom(pp_eunit),
                                                        [erl_syntax:atom(Module),
                                                         erl_syntax:application(
                                                           erl_syntax:atom(eqc_statem),erl_syntax:atom(zip),
                                                           [erl_syntax:variable("Cmds"),
                                                            erl_syntax:list_comp(
                                                              erl_syntax:variable("R"),
                                                              [erl_syntax:generator(
                                                                 erl_syntax:tuple([erl_syntax:underscore(),erl_syntax:variable("R")]),
                                                                 erl_syntax:variable("History"))])])]),
                                  erl_syntax:infix_expr(erl_syntax:variable("Res"),
                                                        erl_syntax:operator("=="),
                                                        erl_syntax:atom(ok))])
                                 ])]) ])])] ++
  % pretty printing
  [] ++
  % local functions
  [ begin
      Vars = [erl_syntax:variable(lists:concat(["X",I])) || I<-lists:seq(1,Arity)],
      erl_syntax:function(erl_syntax:atom(Fun),
                          [ erl_syntax:clause(Vars,[],
                                              [erl_syntax:catch_expr(
                                                 erl_syntax:application(
                                                 erl_syntax:abstract(Mod),
                                                 erl_syntax:abstract(Fun),
                                                 Vars))]
                                             )])
    end || {Mod,Fun,Arity} <- Calls ].

cleanup(Module,[]) ->
  % There is no proper cleanup, but we provide the function
  [erl_syntax:function(erl_syntax:atom(cleanup),
                       [erl_syntax:clause([erl_syntax:variable("_S")],[],
                                           [erl_syntax:atom(none)])])];
cleanup(Module,CleanupTree) ->
  % cleanup function
  [erl_syntax:function(erl_syntax:atom(cleanup),
                       [erl_syntax:clause([erl_syntax:variable("_S")],[],
                                          CleanupTree)])].  


arities(Calls) ->
  lists:usort([ {Mod,Fun,length(Args)} || {Mod,Fun,Args}<-Calls]).

generators(Automata) ->
  Transitions = Automata#fa.tr,
  FromStates = Automata#fa.st,
  EQCStates = 
    [ {From,[ {T,Titem} || {F,{_Abstr,Titems},T}<-Transitions, Titem<-Titems, F==From]} || From<-FromStates],
  TGen = trans_gen(EQCStates),
  [initial_state(Automata#fa.iSt)|TGen].

trans_gen([]) ->
  [];
trans_gen([{From,Transitions}|Rest]) ->
  MergedCalls = 
    lists:foldl(fun({To,#titem{mod = Mod, func = Fun, args = Args}},MCs) ->
                    case lists:keyfind({To,Mod,Fun},1,MCs) of
                      false ->
                        [{{To,Mod,Fun},[Args]}|MCs];
                      {_,Argss} ->
                        [{{To,Mod,Fun},lists:usort([Args|Argss])}|lists:keydelete({To,Mod,Fun},1,MCs)]
                    end
               end,[],lists:sort(Transitions)),
  Trans = 
    lists:foldr(fun({{To,Mod,Fun},Argss},Trs) when length(Argss)>1 ->
                    MergeArgs = 
                      erl_syntax:list(mergeargs(cart(Argss))),
                    [{To,eqccall(Mod,Fun,MergeArgs)}|Trs];
                   ({{To,Mod,Fun},[Args]},Trs) ->
                    [{To,eqccall(Mod,Fun,erl_syntax:abstract(Args))}|Trs]
                end,[],MergedCalls),
  [mkfunc(From,Trans)|trans_gen(Rest)].

cart([]) ->
  [];
cart(LLs) ->
  [ [ hd(X) || X<-LLs] | cart([tl(X) || X<-LLs, length(X) > 1])].

mergeargs([]) ->
  [];
mergeargs([As|Ass]) ->
  case lists:usort(As) of
    [Same] ->
      [erl_syntax:abstract(Same)|mergeargs(Ass)];
    Alts ->
      [erl_syntax:application(
         erl_syntax:atom(oneof),[ erl_syntax:abstract(Alts) ])|mergeargs(Ass)]
  end.
  

rename_states(Automata) ->
  States = Automata#fa.st,
  ErrorStates = Automata#fa.fSt,
  InitialState = Automata#fa.iSt,
  Transitions = Automata#fa.tr,
  StateMap =
    fun(Nr) when Nr==InitialState ->
        state_init;
       (Nr) -> 
        case lists:member(Nr,ErrorStates) of
          true ->
            state_error;
         false ->
            list_to_atom(lists:concat(["state_",Nr]))
        end
    end,
  Automata#fa{st = [ StateMap(S) || S<-States],
              iSt = StateMap(InitialState),
              fSt = [state_error],
              tr = [ {StateMap(F),Call,StateMap(T)} || {F,Call,T}<-Transitions]
           }.
  


eqccall(_Mod,Fun,SyntaxTree) ->
  erl_syntax:tuple([erl_syntax:atom(call),
                    erl_syntax:macro(erl_syntax:variable("MODULE")),erl_syntax:abstract(Fun),SyntaxTree]).

mkfunc(From,Trans) ->
  erl_syntax:function(
    erl_syntax:atom(From),
    [ erl_syntax:clause([erl_syntax:variable("_")],[],
        [erl_syntax:list( [ erl_syntax:tuple([erl_syntax:atom(To),Call]) || {To,Call}<-Trans ] )])
      ]).

mkinclude(String) ->
  erl_syntax:attribute(erl_syntax:atom(include_lib),[erl_syntax:string(String)]).

initial_state(Name) ->
  erl_syntax:function(
    erl_syntax:atom(initial_state),
    [ erl_syntax:clause([],[erl_syntax:atom(Name)])]).



%% code from this should be asbracted and added to the pretty printing part

pp_eunit(SUT,[]) ->
  io:format("\n");
pp_eunit(SUT,Cmds) ->
  io:format("\nnew_test() ->\n"),
  pp(SUT,Cmds).

pp(SUT,[{{set,V,Call},{'EXIT',{Reason,_StackTrace}}}|Rest]) ->
  {call,Mod,Fun,Args} = Call,
  io:format("  ?assertError(~p,~s)",[Reason,eqc_symbolic:pretty_print([],{call,SUT,Fun,Args})]),
  if length(Rest) > 0 -> io:format(",\n"), pp(SUT,Rest);
     true -> io:format(".\n\n")
  end;
pp(SUT,[{{set,V,Call},Return}|Rest]) ->
  {call,Mod,Fun,Args} = Call,
  io:format("  ?assertMatch(~p,~s)",[Return,eqc_symbolic:pretty_print([],{call,SUT,Fun,Args})]),
  if length(Rest) > 0 -> io:format(",\n"), pp(SUT,Rest);
     true -> io:format(".\n\n")
  end.
