:- use_module(library(unifd)).
:- use_module(library(plunit), [begin_tests/1, end_tests/1, run_tests/0]).

:- begin_tests(unifd).

test(simple, all(X-Y == [0-0, 1-1])) :-
    compile_multialg([s(0)-1, s(1)-0], Alg),
    [X, Y] ins 0..2,
    unifd(Alg, s(#(X)) = s(s(s(#(Y))))),
    label([X,Y]).

test(inference, all(C0-C2 == [0-2])) :-
    [C0, C2] ins 0..2,
    compile_multialg([z-0, s(0)-1, s(1)-C2, s(2)-C0], Alg),
    unifd(Alg, z = s(s(s(z)))),
    label([C0,C2]).

test(nondet, X == 1) :-
    X in 0..2,
    compile_multialg([z-0, z-X, s(0)-1], Alg),
    unifd(Alg, z = s(z)),
    label([X]).

test(unwrapped_var,
     [error(instantiation_error(_),
	    context(_, "Variables must be wrapped with #/1"))]) :-
    compile_multialg([z-0], Alg),
    [X, Y] ins 0..2,
    unifd(Alg, X = Y).

:- end_tests(unifd).
