#lang corpse-reviver-paper

Gradually-typed programming languages permit
the incremental addition of static types to untyped programs.
To remain sound,
languages insert run-time checks
at the boundaries between typed and untyped code.
Unfortunately,
performance studies have shown
that the overhead of these checks can be disastrously high,
calling into question the viability of sound gradual typing.
In this paper,
we show that by building on existing work on soft contract verification,
we can reduce or eliminate this overhead.

Our key insight is that while untyped code
cannot be @italic{trusted}
by a gradual type system,
there is no need to consider only the worst case
when optimizing a gradually-typed program.
Instead,
we statically analyze the untyped portions of a gradually-typed program
to prove that almost all of the dynamic checks
implied by gradual type boundaries
cannot fail,
and can be eliminated at compile time.
Our analysis is modular,
and can be applied to any portion of a program.

We evaluate this approach on a dozen existing gradually-typed programs
previously shown to have prohibitive performance overhead---with
a median overhead of @stat:median-overhead-baseline
and up to @stat:worst-case-baseline in the worst case---and
eliminate all overhead in most cases,
suffering only @stat:worst-case-opt overhead in the worst case.