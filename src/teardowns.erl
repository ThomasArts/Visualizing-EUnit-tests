%%%-------------------------------------------------------------------
%%% File    : teardowns.erl
%%% Author  : Simon Thompson
%%% Description : Module to collect traces by running the tests against
%%% the SUT.
%%%
%%% Created : Aug 2011 by Simon Thompson
%%%-------------------------------------------------------------------

-module(teardowns).

-export([collect_bodies/1,get_cleanup/1]).   %% Top-level function

-export([zapNode/1,                          %% Exported as used in HOFs.
	 map_tuple/2,zapWrapper/1]).

%%%-------------------------------------------------------------------
%%
%% Top level function to return the forms in a parsed Eunit file
%% that represent the teardown portion of setups.
%%
%%%-------------------------------------------------------------------

%% File is a String, "Module.erl".

get_cleanup(File) ->
    {ok, Trees} = epp_dodger:parse_file(File),
    Tree = erl_syntax:form_list(Trees),
    NTree = erl_syntax_lib:map(fun zapNode/1,Tree),  %% zap all line number
    NTree2 = zapWrapper(NTree),                      %% information to 0.
    collect_bodies(NTree2).                %% This does the work.

    
%%%-------------------------------------------------------------------
%%
%% Auxiliary functions.
%%
%%%-------------------------------------------------------------------

%% is a node a {setup,...}?

is_setup(Node) ->
    case erl_syntax:type(Node) of
	tuple ->
	    Head = hd(erl_syntax:tuple_elements(Node)),
	    case erl_syntax:type(Head) of
		atom ->
		    erl_syntax:atom_value(Head)==setup orelse
			erl_syntax:atom_value(Head)==foreach;
		_ -> false
	    end;
	_ -> false
    end.

%% Collect the teardown functions from the {setup,...} tests.

collect_teardowns(Tree) ->
    case is_setup(Tree) of
	true ->
	    [lists:nth(3,erl_syntax:tuple_elements(Tree))];
	false -> 
	    lists:concat(lists:map(fun collect_teardowns/1,lists:flatten(erl_syntax:subtrees(Tree))))
    end.

%% Collect all the bodies of the teardown code which
%% have '_' as the argument, so don't use the result
%% of the setup code.

collect_bodies(Tree) ->
  Teardowns = collect_teardowns(Tree),
  Bodies = lists:usort(lists:concat(lists:map(fun get_body/1,Teardowns))),
  [ erl_syntax:catch_expr(erl_syntax:block_expr(Body)) || Body<-Bodies ].

%% Takes teardown code, checks that it ignores argument
%% and returns the corresponding body in a singleton list;
%% otherwise returns an empty list.

get_body(FunExpr) ->
    case erl_syntax:fun_expr_clauses(FunExpr) of
	[Clause] ->
	    case erl_syntax:clause_patterns(Clause) of
		[ Us ] ->
		    case erl_syntax:type(Us) of
			underscore -> [erl_syntax:clause_body(Clause)];
			_ -> []
		    end;
		_ -> []
	    end;
	_ -> []
    end.

%% Set the position information in a node to be 0.
    
zapNode(Tree) ->
    erl_syntax:set_pos(Tree,0).



%% Yuck! Breaks the abstraction of erl_syntax representation
%% to zap the line number in leaf nodes to zero. 

map_tuple(F,T) ->
    list_to_tuple(lists:map(F,(tuple_to_list(T)))).

zapWrapper({wrapper,_,_,{A,_,C}}) -> {A,0,C};

zapWrapper(T) -> 
  case is_tuple(T) of
      true ->
	  map_tuple(fun zapWrapper/1,T);
      false ->
	  case is_list(T) of
	      true ->
		  lists:map(fun zapWrapper/1,T);
	      false -> T
	  end
  end.
    
