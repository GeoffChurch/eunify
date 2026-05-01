:- module(eunify_naive, [pairs_to_alg/2, apply_alg/3]).

:- reexport(library(eunify)).
:- use_module(library(eunify/shared)).

%%%% Definition %%%%

pairs_to_alg --> pairs_to_alg_.

%%%% Application %%%%

apply_alg(Alg, Term, Out) =>
    $(term_functor_args(Term, F, Ins)),
    $(rb_lookup(F, Cases, Alg)),
    member(Ins-Out, Cases).
