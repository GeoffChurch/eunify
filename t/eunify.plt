:- use_module(library(eunify)).
:- use_module(library(clpfd)).

:- begin_tests(eunify).

test(simple, all(X-Y == [0-0, 1-1])) :-
    pairs_to_clpfd_alg([s(0)-1, s(1)-0], Alg),
    [X, Y] ins 0..2,
    eunify(Alg, s(?(X)) = s(s(s(?(Y))))),
    label([X,Y]).

test(simple_fail) :-
    pairs_to_clpfd_alg([s(0)-1, s(1)-0], Alg),
    X in 0..2,
    eunify(Alg, s(?(X)) = s(s(?(X)))),
    \+ label([X]).

test(inference, all(C0-C2 == [0-2])) :-
    [C0, C2] ins 0..2,
    pairs_to_clpfd_alg([z-0, s(0)-1, s(1)-C2, s(2)-C0], Alg),
    eunify(Alg, z = s(s(s(z)))),
    label([C0,C2]).

test(nondet, X == 1) :-
    X in 0..2,
    pairs_to_clpfd_alg([z-0, z-X, s(0)-1], Alg),
    eunify(Alg, z = s(z)),
    label([X]).

test(unwrapped_var,
     [error(instantiation_error(_),
	    context(_, "Variables must be wrapped with (?)/1"))]) :-
    pairs_to_clpfd_alg([z-0], Alg),
    [X, Y] ins 0..2,
    eunify(Alg, X = Y).

:- end_tests(eunify).
