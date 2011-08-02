%%% File    : updateapta_eqc.erl
%%% Author  : Pablo Lamela <>
%%% Description : QuickCheck properties for the update of apta.erl
%%% Created :  2 Aug 2010 by Pablo Lamela <>

-module(updateapta_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("visualize.hrl").

-compile([export_all]).

trace() -> ?LET(Automata, rfsa_eqc:automata(),
				?LET(S, rfsa_eqc:automata_sample(Automata),
					 begin
						 Pos = [Seq || {Seq,pos} <- S],
						 Neg = [Seq || {Seq,neg} <- S],
						 Sample = {Pos,Neg},
						 Sample
					 end)).

prop_test() ->
	?FORALL(Sample, trace(),
			begin
				Fun = fun (X) -> {test, X, test} end,
				Old = old_bluefringe_apta:generateApta(Sample, Fun),
				InvFun = fun ({test, X, test}) -> X end,
				New = bluefringe_apta:generateApta(Sample, Fun),
				AdaptedOld = Old#fa{tr = [{Ori, {Tran, [InvFun(Tran)]}, Dest} || {Ori, Tran, Dest} <- Old#fa.tr]},
				equals(New, AdaptedOld)
			end).

prop_test2() ->
	fails(?FORALL(Sample, trace(),
			begin
				Fun = fun (X) -> {test} end,
				Old = old_bluefringe_apta:generateApta(Sample, Fun),
				New = bluefringe_apta:generateApta(Sample, Fun),
				AdaptedNew = New#fa{tr = [{Ori, Tran, Dest} || {Ori, {Tran, _Info}, Dest} <- New#fa.tr]},
				equals(AdaptedNew, Old)
			end)).