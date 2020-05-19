#lang scribble/acmart

Gradually-typed programming languages permit the incremental addition
of static types to untyped programs.  To remain sound, languages
insert run-time checks between the boundaries of typed and untyped
code.  Unfortunately, performance studies have shown that the overhead
of these checks can be disastrously high, calling into question the
pragmatics of sound gradual typing.  In this paper, we show that
existing work on soft contract verification is uniquely suited to reduce
or eliminate this overhead.

Our key insight is that while untyped code cannot be @emph{trusted}
by a gradual type system, there is no need to consider only the
worst case when optimizing a complete gradually-typed
program. Instead, we statically analyze the untyped portions of
a gradually-typed program to prove that almost all of the dynamic
checks implied by gradual type boundaries cannot fail, and can
be eliminated at compile-time.

We evaluate this approach on an existing benchmark of gradually-typed
programs, previously shown to have prohibitive performance overhead,
and reduce it by multiple orders of magnitude.
