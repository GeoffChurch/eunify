:- module(eunify, [eunify/2, pairs_to_alg/3, pairs_to_clpfd_alg/2]).

term_functor_args(Term, N/A, Args) :-
    $(functor(Term, N, A)),
    $(Term =.. [N|Args]).

apply_op(i_o_state(InVars, OutVar, State), Ins, Out) =>
    copy_term(i_o_state(InVars, OutVar, State),
              i_o_state(Ins,    Out,    State)).

apply_alg(Alg, Term, Out) =>
    $(term_functor_args(Term, F, Args)),
    $(rb_lookup(F, Mop, Alg)),
    apply_op(Mop, Args, Out).

:- meta_predicate cata(2, ?, ?).
cata(F, A, B) :-
    $(rb_empty(Seen)),
    cata_(F, Seen, A, B).

:- meta_predicate cata_(2, +, ?, ?).
cata_(_, _, Var, _), var(Var) =>
    throw(error(instantiation_error(Var),
          context(_Loc, "Variables must be wrapped with (?)/1"))).
cata_(_, _, ?(A), B) => B = A.
cata_(F, Seen, A, B) =>
    rb_insert_new(Seen, A, B, Seen1)
    -> $(same_functor(A, C)), % Apply constraint early.
       call(F, C, B),
       mapargs(cata_(F, Seen1), A, C)
    ;  $(rb_lookup(A, B, Seen)). % Tie the knot.

eunify(Alg, X = Y) =>
    cata(apply_alg(Alg), X, Z),
    cata(apply_alg(Alg), Y, Z).


%%% Helpers for constructing an algebra %%%

pairs_to_clpfd_alg --> pairs_to_alg(or_and_eq(#\/, #/\, #=)).

pairs_to_alg(Type, Pairs, CAlg) :-
    $(pairs_functions_ops(Pairs, Functions, Ops)),
    $(pairs_keys_values(FToMopPairs, Functions, Ops)),
    $(group_pairs_by_key(FToMopPairs, CAlgList)),
    $(list_to_rbtree(CAlgList, CAlgRB)),
    $(rb_map(CAlgRB, compile_op(Type), CAlg)).

compile_op(Type, Cases, i_o_state(IVars, OVar, State)) :-
    $(Cases = [Is-_|_]),
    $(length(Is, Arity)),
    $(length(IVars, Arity)),
    $(term_variables_excluding(Cases, State, [OVar|IVars])),
    $(post_op_constraint(Type, Cases, IVars, OVar)).

post_op_constraint(or_and_eq(Or, And, Eq), Cases, IVars, OVar) =>
    $(maplist(case_constraint(or_and_eq(Or, And, Eq), IVars, OVar), Cases, [C|Cs])),
    $(foldl(cons(Or), Cs, C, Expr)),
    $(call(Expr)).

term_variables_excluding(Term, Vars, Exclude) :-
    $(term_variables(Term, All)),
    $(list_to_ord_set(All, AllS)),
    $(list_to_ord_set(Exclude, ExcludeS)),
    $(ord_subtract(AllS, ExcludeS, Vars)).

case_constraint(or_and_eq(_, And, Eq), Ins, Out, Args-Value, Expr) =>
    $(maplist(cons(Eq), Ins, Args, Conjs)),
    cons(Eq, Out, Value, Conj1),
    $(foldl(cons(And), Conjs, Conj1, Expr)).

pairs_functions_ops(Alg, Functions, Ops) :-
    $(sort(Alg, SortedAlg)),
    $(pairs_keys_values(SortedAlg, PreImage, Outputs)),
    $(maplist(term_functor_args, PreImage, Functions, Inputs)),
    $(pairs_keys_values(Ops, Inputs, Outputs)).

cons(Op, A, B, C) :-
    $(functor(C, Op, 2)),
    $(arg(1, C, A)),
    $(arg(2, C, B)).
