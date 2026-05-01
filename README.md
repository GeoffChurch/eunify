# `eunify`

`eunify` implements [E-unification][wiki] for SWI-Prolog. Set the free algebra
free with `eunify` today.

[wiki]:
  https://en.wikipedia.org/wiki/Unification_(computer_science)#E-unification

The core predicate is `eunify`, implemented as

```prolog
eunify(Alg, X = Y) =>
    cata(Alg, X, Z),
    cata(Alg, Y, Z).
```

It uses `cata` to recursively evaluate both `X` and `Y` to `Z` using `Alg`.

`eunify` is backend-agnostic (it's easy to add your own), and currently provides
two backends:

| Backend | Module                  | Use Case       |
| ------- | ----------------------- | -------------- |
| CLP(FD) | `library(eunify/clpfd)` | Large problems |
| Naive   | `library(eunify/naive)` | Small problems |

Each backend module exports `pairs_to_alg/2` to compile an algebra and
`apply_alg/3` to wrap it in a callable. For example, the natural numbers modulo
2 are described by the following algebra:

<table>
<tr><th>CLP(FD)</th><th>Naive</th></tr>
<tr><td valign="top">

```prolog
?- use_module(library(eunify/clpfd)).
true.

?- pairs_to_alg([z-0, s(0)-1, s(1)-0], _Alg),
   rb_visit(_Alg, Alg).
Alg = [s/1-i_o_state([_A], _B, []),
       z/0-i_o_state([], 0, [])],
clpfd:in(_C, ..(0, 1)),
clpfd: #<==>(#\/(_C, _D), 1),
clpfd: #<==>(#/\(_E, _F), _C),
clpfd:in(_E, ..(0, 1)),
clpfd: #<==>(#=(_A, 1), _E),
clpfd: #<==>(#=(_A, 0), _G),
clpfd:in(_F, ..(0, 1)),
clpfd: #<==>(#=(_B, 0), _F),
clpfd: #<==>(#=(_B, 1), _H),
clpfd:in(_D, ..(0, 1)),
clpfd: #<==>(#/\(_G, _H), _D),
clpfd:in(_G, ..(0, 1)),
clpfd:in(_H, ..(0, 1)).
```

</td><td valign="top">

```prolog
?- use_module(library(eunify/naive)).
true.

?- pairs_to_alg([z-0, s(0)-1, s(1)-0], _Alg),
   rb_visit(_Alg, Alg).
Alg = [s/1-[[0]-1, [1]-0],
       z/0-[[]-0]].
```

</td></tr>
</table>

The CLP(FD) backend compiles each operation's cases into a single
`i_o_state(InVars, OutVar, State)` term. The cases are compiled into a single
CLP(FD) constraint on `InVars` and `OutVar`. `z/0` becomes
`i_o_state([], 0, [])` (zero inputs, output is `0`, stateless), and `s/1`
becomes `i_o_state([_A], _B, [])` (input is `_A`, output is `_B`, stateless),
where `_A` and `_B` are constrained by
`(_A #= 0 #/\ _B #= 1) #\/ (_A #= 1 #/\ _B #= 0)` in which the two cases are
apparent. CLP(FD) spaghettifies this into an unreadable but equivalent mess. The
naive backend just keeps the raw `Inputs-Output` cases in a per-operation list.

Both backends accept algebras that contain variables. The CLP(FD) backend uses
`copy_term` to apply operations to arguments without binding the parameters
themselves, but we want variables _in the algebra_ to be treated as
globally-scoped existential variables, so it uses the `state` slot of
`i_o_state` to bind these variables between the operation's definitional
skeleton and its application.

<table>
<tr><th>CLP(FD)</th><th>Naive</th></tr>
<tr><td valign="top">

```prolog
?- pairs_to_alg([x-X, y-Y, f(0,1)-1], _Alg),
   rb_visit(_Alg, Alg).
Alg = [f/2-i_o_state([0, 1], 1, []),
       x/0-i_o_state([], X, [X]),
       y/0-i_o_state([], Y, [Y])],
clpfd:in(X, ..(inf, sup)),
clpfd:in(Y, ..(inf, sup)).
```

</td><td valign="top">

```prolog
?- pairs_to_alg([x-X, y-Y, f(0,1)-1], _Alg),
   rb_visit(_Alg, Alg).
Alg = [f/2-[[0, 1]-1],
       x/0-[[]-X],
       y/0-[[]-Y]].
```

</td></tr>
</table>

E-unification then determines values for `X` and `Y`, grounding the algebra:

<table>
<tr><th>CLP(FD)</th><th>Naive</th></tr>
<tr><td valign="top">

```prolog
?- pairs_to_alg([x-X, y-Y, f(0,1)-1], _Alg),
   rb_visit(_Alg, Alg),
   eunify(apply_alg(_Alg), f(x, y) = ?(Z)).
X = 0,
Y = Z, Z = 1,
Alg = [f/2-i_o_state([0, 1], 1, []),
       x/0-i_o_state([], 0, [0]),
       y/0-i_o_state([], 1, [1])].
```

</td><td valign="top">

```prolog
?- pairs_to_alg([x-X, y-Y, f(0,1)-1], _Alg),
   rb_visit(_Alg, Alg),
   eunify(apply_alg(_Alg), f(x, y) = ?(Z)).
X = 0,
Y = Z, Z = 1,
Alg = [f/2-[[0, 1]-1],
       x/0-[[]-0],
       y/0-[[]-1]].
```

</td></tr>
</table>

A technical note: the operations in the algebra can be nondeterministic (`f(0)`
might not map to anything while `f(1)` might map to both `2` and `3`). For
example, the following succeeds under either backend because `f(a, a)` can
evaluate to `f(0, 1)` and hence to `1`:

```prolog
?- pairs_to_alg([a-0, a-1, f(0,1)-1], _Alg),
   eunify(apply_alg(_Alg), f(a, a) = ?(1)).
```

## Install

`?- pack_install(eunify).`
