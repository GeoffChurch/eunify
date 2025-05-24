# `eunify`

`eunify` implements [E-unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#E-unification) for SWI-Prolog. Set the free algebra free with `eunify` today.

The core predicate is `eunify`, implemented as
```prolog
eunify(Alg, X = Y) =>
    cata(apply_alg(Alg), X, Z),
    cata(apply_alg(Alg), Y, Z).
```

It uses `cata` to recursively evaluate both `X` and `Y` to `Z` using the user-defined algebra.

In the case of a finite algebra (i.e. the functions map into a finite carrier set), we can leverage the CLP(FD) constraint solver. For example, the natural numbers modulo 2 are described by the following algebra:
```prolog
?- pairs_to_clpfd_alg([z-0, s(0)-1, s(1)-0], _Alg), rb_visit(_Alg, Pairs).
Pairs = [s/1-i_o_state([_C], _D, []), z/0-i_o_state([], 0, [])],
_E in 0..1,
_E#\/_F#<==>1,
_G#/\_H#<==>_E,
_G in 0..1,
_C#=1#<==>_G,
_C#=0#<==>_I,
_H in 0..1,
_D#=0#<==>_H,
_D#=1#<==>_J,
_F in 0..1,
_I#/\_J#<==>_F,
_I in 0..1,
_J in 0..1.
```
Looking at `Pairs`, `z/0` maps to `i_o_state([], 0, [])`, which describes a function that takes no arguments (the first `[]`) and returns a `0`. `s/1` maps to `i_o_state([_C], _D, [])`, which describes a function that takes a single argument (bound to `_C`) and returns `_D`. `_C` and `_D` are related by the list of constraints printed out under `Pairs`. Both of the `i_o_state` terms have an empty list as their "state". This is because the algebra itself has no variables, unlike the following, which binds logic variables `X` and `Y` to the internal identifiers `x` and `y`:
```prolog
?- pairs_to_clpfd_alg([x-X, y-Y, f(0,1)-1], _Alg), rb_visit(_Alg, Pairs).
Pairs = [f/2-i_o_state([0, 1], 1, []), x/0-i_o_state([], X, [X]), y/0-i_o_state([], Y, [Y])],
X in inf..sup,
Y in inf..sup.
```
We can determine values for `X` and `Y`, thereby grounding the states:
```prolog
?- pairs_to_clpfd_alg([x-X, y-Y, f(0,1)-1], _Alg), rb_visit(_Alg, Pairs), eunify(_Alg, f(x, y) = ?(Z)).
X = 0,
Y = Z, Z = 1,
Pairs = [f/2-i_o_state([0, 1], 1, []), x/0-i_o_state([], 0, [0]), y/0-i_o_state([], 1, [1])].
```

A technical note: the operations in the algebra can be partial (undefined for some inputs) and/or nondeterministic (`f(1)` could nondeterministically return `2` or `3`). For example, the following succeeds because `f(a, a)` can evaluate to `f(0, 1)` and hence to `1`: `pairs_to_clpfd_alg([a-0, a-1, f(0,1)-1], _Alg), eunify(_Alg, f(a, a) = ?(1))`.

To install: `?- pack_install(eunify)`.