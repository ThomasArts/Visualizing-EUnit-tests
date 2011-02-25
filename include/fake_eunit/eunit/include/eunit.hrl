%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: eunit.hrl 337 2009-03-09 08:38:28Z rcarlsson $
%%
%% Copyright (C) 2004-2006 Mickaël Rémond, Richard Carlsson

%% Including this file turns on testing and defines TEST, unless NOTEST
%% is defined before the file is included. If both NOTEST and TEST are
%% already defined, then TEST takes precedence, and NOTEST will become
%% undefined.
%% 
%% If NODEBUG is defined before this file is included, the debug macros
%% are disabled, unless DEBUG is also defined, in which case NODEBUG
%% will become undefined. NODEBUG also implies NOASSERT, unless testing
%% is enabled.
%%
%% If including this file causes TEST to be defined, then NOASSERT will
%% be undefined, even if it was previously defined and even if NODEBUG
%% is defined. If both ASSERT and NOASSERT are defined before the file
%% is included, then ASSERT takes precedence, and NOASSERT will become
%% undefined regardless of TEST.
%% 
%% After including this file, EUNIT will be defined if and only if TEST
%% is defined.

-ifndef(EUNIT_HRL).
-define(EUNIT_HRL, true).

%% All macros should be available even if testing is turned off, and
%% should preferably not require EUnit to be present at runtime.
%% 
%% We must use fun-call wrappers ((fun () -> ... end)()) to avoid
%% exporting local variables, and furthermore we only use variable names
%% prefixed with "__", that hopefully will not be bound outside the fun.

%% A generic let-macro is particularly useful when writing test cases.
%% It is more compact than 'begin X = Y, Z end', and guarantees that
%% X gets a new, local binding.
%% (Note that lowercase 'let' is a reserved word.)
-ifndef(LET).
-define(LET(X,Y,Z), ((fun(X)->(Z)end)(Y))).
-endif.

%% It is important that testing code is short and readable.
%% An if-then-else macro can make some code much more compact.
%% Compare:  case f(X) of true->g(X); false->h(X) end
%%     and:  ?IF(f(X), g(Y), h(Z))
-ifndef(IF).
-define(IF(B,T,F), (case (B) of true->(T); false->(F) end)).
-endif.

%% This macro yields 'true' if the value of E matches the guarded
%% pattern G, otherwise 'false'.
-ifndef(MATCHES).
-define(MATCHES(G,E), (case (E) of G -> true; _ -> false end)).
-endif.

%% This macro can be used at any time to check whether or not the code
%% is currently running directly under eunit. Note that it does not work
%% in secondary processes if they have been assigned a new group leader.
-ifndef(UNDER_EUNIT).
-define(UNDER_EUNIT,false).
-endif.

%% The plain assert macro should be defined to do nothing if this file
%% is included when debugging/testing is turned off.
-ifndef(assert).
-define(assert(BoolExpr), BoolExpr).
-endif.

-define(assertNot(BoolExpr), BoolExpr).

-define(test(Expr), Expr).

%% This is mostly a convenience which gives more detailed reports.
%% Note: Guard is a guarded pattern, and can not be used for value.
-define(assertMatch(Guard,Expr), Expr).
-define(_assertMatch(Guard,Expr), ?assertMatch(Guard,Expr)).
-define(assertEqual(Expect,Expr), Expr).
-define(_assertEqual(Expect,Expr), ?assertEqual(Expect,Expr)).
-define(assertException(Class, Term, Expr), {fsm_eunit_parser_negative, Expr}).
-define(_assertException(Class, Term, Expr), ?assertException(Class, Term, Expr)).

-define(assertError(Term, Expr), ?assertException(error, Term, Expr)).
-define(_assertError(Term, Expr), ?assertError(Term, Expr)).
-define(assertExit(Term, Expr), ?assertException(exit, Term, Expr)).
-define(_assertExit(Term, Expr), ?assertExit(Term, Expr)).
-define(assertThrow(Term, Expr), ?assertException(throw, Term, Expr)).
-define(_assertThrow(Term, Expr), ?assertThrow(Term, Expr)).

%% Macros for running operating system commands. (Note that these
%% require EUnit to be present at runtime, or at least eunit_lib.)

%% these can be used for simply running commands in a controlled way
-define(cmdStatus(N, Cmd), os:command(Cmd)).
-define(cmd(Cmd), ?cmdStatus(0, Cmd)).

%% these are only used for testing; they always return 'ok' on success,
%% and have no effect if debugging/testing is turned off
-define(assertCmdStatus(N, Cmd), cmdStatus(N, Cmd)).
-define(assertCmd(Cmd), ?assertCmdStatus(0, Cmd)).
-define(assertCmdOutput(T, Cmd), cmd(Cmd)).

%% Macros to simplify debugging (in particular, they work even when the
%% standard output is being redirected by EUnit while running tests)

-define(debugMsg(S), ok).
-define(debugHere, ok).
-define(debugFmt(S, As), ok).
-define(debugVal(E), (E)).
-define(debugTime(S,E), (E)).

-endif. % EUNIT_HRL

