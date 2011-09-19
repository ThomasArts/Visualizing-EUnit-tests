-module(frequency_eqc).

-include_lib("eqc/include/eqc.hrl").

-include_lib("eqc/include/eqc_fsm.hrl").

-compile(export_all).

initial_state() -> state_init.

state_init(_) ->
    [{state_error, {call, ?MODULE, allocate, []}},
     {state_error, {call, ?MODULE, stop, []}},
     {state_1, {call, ?MODULE, start, [oneof([[], [1]])]}}].

state_1(_) ->
    [{state_init, {call, ?MODULE, allocate, []}},
     {state_init, {call, ?MODULE, stop, []}},
     {state_error, {call, ?MODULE, start, [[1, 2]]}}].

state_error(_) -> [].

precondition(_From, _To, _S, {call, _, allocate, []}) ->
    true;
precondition(_From, _To, _S, {call, _, start, [_]}) ->
    true;
precondition(_From, _To, _S, {call, _, stop, []}) ->
    true.

initial_state_data() -> [].

next_state_data(_From, _To, S, _V,
		{call, _, allocate, []}) ->
    S;
next_state_data(_From, _To, S, _V,
		{call, _, start, [_]}) ->
    S;
next_state_data(_From, _To, S, _V,
		{call, _, stop, []}) ->
    S.

postcondition(_From, state_error, _S, _Call, R) ->
    case R of
      {'EXIT', _} -> true;
      _ -> false
    end;
postcondition(_From, _To, _S, {call, _, allocate, []},
	      R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end;
postcondition(_From, _To, _S, {call, _, start, [_]},
	      R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end;
postcondition(_From, _To, _S, {call, _, stop, []}, R) ->
    case R of
      {'EXIT', _} -> false;
      _ -> true
    end.

prop_frequency() ->
    ?FORALL(Cmds, (commands(?MODULE)),
	    begin
	      {History, S, Res} = run_commands(?MODULE, Cmds),
	      cleanup(S),
	      ?WHENFAIL((bluefringe_fsm:pp_eunit(eqc_statem:zip(Cmds,
								[R
								 || {_, R}
									<- History]))),
			(Res == ok))
	    end).

allocate() -> catch frequency:allocate().

start(X1) -> catch frequency:start(X1).

stop() -> catch frequency:stop().

cleanup(_S) -> catch begin stop() end.

