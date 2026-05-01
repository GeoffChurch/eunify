:- module(eunify_shared, [term_functor_args/3,
                          pairs_to_alg_/2,
                          cons/4]).

term_functor_args(Term, N/A, Args) :-
    $(functor(Term, N, A)),
    $(Term =.. [N|Args]).

pairs_to_alg_(Pairs, AlgRB) :-
    $(pairs_functions_ops(Pairs, Functions, Ops)),
    $(pairs_keys_values(FToMopPairs, Functions, Ops)),
    $(group_pairs_by_key(FToMopPairs, AlgList)),
    $(list_to_rbtree(AlgList, AlgRB)).

pairs_functions_ops(Alg, Functions, Ops) :-
    $(sort(Alg, SortedAlg)),
    $(pairs_keys_values(SortedAlg, PreImage, Outputs)),
    $(maplist(term_functor_args, PreImage, Functions, Inputs)),
    $(pairs_keys_values(Ops, Inputs, Outputs)).

cons(Op, A, B, C) :-
    $(functor(C, Op, 2)),
    $(arg(1, C, A)),
    $(arg(2, C, B)).
