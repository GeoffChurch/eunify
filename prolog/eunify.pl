:- module(eunify, [eunify/2, cata/3]).

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

:- meta_predicate eunify(2, ?).
eunify(Alg, X = Y) =>
    cata(Alg, X, Z),
    cata(Alg, Y, Z).
