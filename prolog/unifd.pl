:- module(unifd,
      [
          compile_multialg/2,
          unifd/2
      ]).
:- reexport(library(clpfd)).

:- use_module(library(apply), [maplist/4, foldl/4, maplist/3]).
:- use_module(library(pairs), [pairs_keys_values/3, 
                   group_pairs_by_key/2]).
:- use_module(library(rbtrees), [list_to_rbtree/2, rb_map/3, 
                 rb_lookup/3, rb_empty/1, 
                 rb_insert_new/4]).
:- use_module(library(terms), [same_functor/2, mapargs/3]).

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

compile_multiop_(Arity, Cases, forall([OVar|IVars], Expr)) :-
    $(length(IVars, Arity)),
    $(maplist(compile_case(IVars, OVar), Cases, [C|Cs])),
    $(foldl(cons(#\/), Cs, C, Expr)).

apply_multiop(forall(Vars, Expr), Is, O) =>
    $(copy_term(Vars, Expr, [O|Is], Expr_)),
    call(Expr_).

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
