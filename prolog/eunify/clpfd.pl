:- module(eunify_clpfd, [pairs_to_alg/2, apply_alg/3]).

:- reexport(library(eunify)).
:- use_module(library(clpfd)).
:- use_module(library(eunify/shared)).

%%%% Definition %%%%

pairs_to_alg(Pairs, Alg) :-
    $(pairs_to_alg_(Pairs, AlgRB)),
    $(rb_map(AlgRB, compile_op, Alg)).

compile_op(Cases, i_o_state(IVars, OVar, State)) :-
    $(Cases = [Is-_|_]),
    $(length(Is, Arity)),
    $(length(IVars, Arity)),
    $(term_variables(Cases, State)),
    $(post_op_constraint(Cases, IVars, OVar)).

post_op_constraint(Cases, IVars, OVar) =>
    $(maplist(case_constraint(IVars, OVar), Cases, [C|Cs])),
    $(foldl(cons(#\/), Cs, C, Expr)),
    $(call(Expr)).

case_constraint(Ins, Out, Inputs-Output, Expr) =>
    $(maplist(cons(#=), Ins, Inputs, Conjs)),
    $(cons(#=, Out, Output, Conj1)),
    $(foldl(cons(#/\), Conjs, Conj1, Expr)).

%%%% Application %%%%

apply_alg(Alg, Term, Out) =>
    $(term_functor_args(Term, F, Ins)),
    $(rb_lookup(F, Mop, Alg)),
    apply_op(Mop, Ins, Out).

apply_op(i_o_state(InVars, OutVar, State), Ins, Out) =>
    copy_term(i_o_state(InVars, OutVar, State),
              i_o_state(Ins,    Out,    State)).
