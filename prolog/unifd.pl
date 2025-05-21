:- module(unifd,
      [
          compile_multialg/2,
          unifd/2
      ]).
:- reexport(library(clpfd)).
:- autoload(library(apply), [maplist/4, foldl/4, maplist/3]).
:- autoload(library(ordsets), [list_to_ord_set/2, ord_subtract/3]).
:- autoload(library(pairs), [pairs_keys_values/3, group_pairs_by_key/2]).
:- autoload(library(rbtrees), [list_to_rbtree/2, rb_map/3, rb_lookup/3, 
                               rb_empty/1, rb_insert_new/4]).
:- autoload(library(terms), [same_functor/2, mapargs/3]).


cons(Op, A, B, C) :-
    $(functor(C, Op, 2)),
    $(arg(1, C, A)),
    $(arg(2, C, B)).

compile_case(Ins, Out, Args-Value, Expr) =>
    $(maplist(cons(#=), Ins, Args, Conjs)),
    $(foldl(cons(#/\), Conjs, Out #= Value, Expr)).

compile_multiop(Cases, Cmop) :-
    $(Cases = [Is-_|_]),
    $(length(Is, Arity)),
    $(compile_multiop_(Arity, Cases, Cmop)).

compile_multiop_(Arity, Cases, us_es([OVar|IVars], Existentials)) :-
    $(length(IVars, Arity)),
    $(term_variables_excluding(Cases, Existentials, [OVar|IVars])),
    $(maplist(compile_case(IVars, OVar), Cases, [C|Cs])),
    $(foldl(cons(#\/), Cs, C, Expr)),
    $(call(Expr)).

term_variables_excluding(Term, Vars, Exclude) :-
    $(term_variables(Term, All)),
    $(list_to_ord_set(All, AllS)),
    $(list_to_ord_set(Exclude, ExcludeS)),
    $(ord_subtract(AllS, ExcludeS, Vars)).


apply_multiop(us_es(Us, Es), Is, O) =>
    $(copy_term(Us-Es, [O|Is]-Es)).

compile_multialg(Alg, CAlg) :-
    $(sort(Alg, SortedAlg)),
    $(pairs_keys_values(SortedAlg, PreImage, Outputs)),
    $(maplist(term_functor_args, PreImage, Functions, Inputs)),
    $(pairs_keys_values(Mops, Inputs, Outputs)),
    $(pairs_keys_values(FToMopPairs, Functions, Mops)),
    $(group_pairs_by_key(FToMopPairs, CAlgList)),
    $(list_to_rbtree(CAlgList, CAlgRB)),
    $(rb_map(CAlgRB, compile_multiop, CAlg)).

term_functor_args(Term, N/A, Args) :-
    $(functor(Term, N, A)),
    $(Term =.. [N|Args]).

apply_multialg(Alg, Term, Out) =>
    $(term_functor_args(Term, F, Args)),
    $(rb_lookup(F, Mop, Alg)),
    apply_multiop(Mop, Args, Out).

:- meta_predicate cata(2, ?, ?).
cata(F) --> { $(rb_empty(Seen)) }, cata_(F, Seen).

:- meta_predicate cata_(2, +, ?, ?).
cata_(_, _, Var, _), var(Var) =>
    throw(error(instantiation_error(Var),
          context(_Loc, "Variables must be wrapped with (#)/1"))).
cata_(_, _, #(A), B) => #(B) #= #(A).
cata_(F, S, A, B) => % TODO do we even need cycle safety?
    rb_insert_new(S, A, B, S1)
    -> $(same_functor(A, C)), % Apply constraint early.
       call(F, C, B),
       mapargs(cata_(F, S1), A, C)
    ;  $(rb_lookup(A, B, S)). % Tie the knot.

unifd(Alg, X = Y) =>
    cata(apply_multialg(Alg), X, Z),
    cata(apply_multialg(Alg), Y, Z).
