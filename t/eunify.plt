:- use_module(library(eunify)).
:- use_module(library(eunify/clpfd), []).
:- use_module(library(eunify/naive), []).
:- use_module(library(clpfd)).

:- begin_tests(eunify).

clpfd_backend(Pairs, Alg, eunify_clpfd:apply_alg(Alg)) :-
    eunify_clpfd:pairs_to_alg(Pairs, Alg).

naive_backend(Pairs, Alg, eunify_naive:apply_alg(Alg)) :-
    eunify_naive:pairs_to_alg(Pairs, Alg).

:- meta_predicate eunify_with(3, ?, ?).
eunify_with(Backend, Pairs, Eq) :-
    call(Backend, Pairs, _Alg, Apply),
    eunify(Apply, Eq).

test(clpfd_simple, all(X-Y == [0-0, 1-1])) :-
    [X, Y] ins 0..2,
    eunify_with(clpfd_backend, [s(0)-1, s(1)-0], s(?(X)) = s(s(s(?(Y))))),
    label([X,Y]).

test(clpfd_simple_fail) :-
    X in 0..2,
    eunify_with(clpfd_backend, [s(0)-1, s(1)-0], s(?(X)) = s(s(?(X)))),
    \+ label([X]).

test(clpfd_inference, all(C0-C2 == [0-2])) :-
    [C0, C2] ins 0..2,
    eunify_with(clpfd_backend, [z-0, s(0)-1, s(1)-C2, s(2)-C0], z = s(s(s(z)))),
    label([C0,C2]).

test(clpfd_nondet, X == 1) :-
    X in 0..2,
    eunify_with(clpfd_backend, [z-0, z-X, s(0)-1], z = s(z)),
    label([X]).

test(clpfd_unwrapped_var,
     [error(instantiation_error(_),
	    context(_, "Variables must be wrapped with (?)/1"))]) :-
    eunify_with(clpfd_backend, [z-0], _ = _).

test(naive_simple, all(X-Y == [0-0, 1-1])) :-
    eunify_with(naive_backend, [s(0)-1, s(1)-0], s(?(X)) = s(s(s(?(Y))))).

test(naive_simple_fail) :-
    \+ eunify_with(naive_backend, [s(0)-1, s(1)-0], s(?(X)) = s(s(?(X)))).

test(naive_inference, all(C0-C2 == [0-2])) :-
    eunify_with(naive_backend, [z-0, s(0)-1, s(1)-C2, s(2)-C0], z = s(s(s(z)))).

test(naive_nondet, all(X == [1])) :-
    eunify_with(naive_backend, [z-0, z-X, s(0)-1], z = s(z)).

test(naive_unwrapped_var,
     [error(instantiation_error(_),
	    context(_, "Variables must be wrapped with (?)/1"))]) :-
    eunify_with(naive_backend, [z-0], _ = _).

:- end_tests(eunify).
