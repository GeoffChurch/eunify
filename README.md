# `unifd`

`unifd` implements [semantic unification](https://en.wikipedia.org/wiki/Unification_(computer_science)#E-unification) in finite domains for SWI-Prolog.

For example: find `X` and `Y` such that `s(s(z))` unifies with `z`, given that `s(0)` evaluates to `1`, `s(1)` evaluates to `2`, and `s(X)` evaluates to `Y`:
```prolog
?- [X,Y] ins -10..10, compile_multialg([z-0, s(0)-1, s(1)-2, s(X)-Y], Alg), unifd(Alg, s(s(s(z))) = z), label([X,Y]).
X = Y, Y = 0, % This solution only works in multialgebras, since s(0) can already evaluate to 1.
Alg = ... ;
X = 2,
Y = 0,
Alg = ... .
```

Variables in the unificands must be wrapped with `(#)/1`, so e.g. to find that the sole predecessor of `0` in mod-3 arithmetic is `2`:
```prolog
?- X in -10..10, compile_multialg([z-0, s(0)-1, s(1)-2, s(2)-0], Alg), unifd(Alg, s(#(X)) = z).
X = 2,
Alg = ... .
```

TODO: Enable enforcement of properties like determinism and commutativity. Maybe by exposing the abstract formal parameters of the compiled multioperation.

To install: `?- pack_install(unifd)`.