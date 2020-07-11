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


@; REMOVE

@(require "private/stats.rkt")

We evaluate this approach on a dozen existing gradually-typed
programs previously shown to have prohibitive performance
overhead---with a median overhead of @stats:median-overhead-baseline and up to
@stats:worst-case-baseline in the worst case---and eliminate all overhead
in most cases and suffer @stats:worst-case-opt in the worst case.

@;

For example, Typed Racket 7.7 runs @stats:percent-two-times-overhead-baseline of benchmark
configurations with less than $2\times$ slowdown.
With \tool, $95\%$ of
benchmark configurations have less than @stats:95-percentile-overhead-opt slowdown
compared to the fully-untyped configuration.
As this plot makes clear, \tool reduces overhead to nearly zero in almost
all cases, and completely eliminates all overheads over @stats:worst-case-opt .

@;

Figure~\ref{fig:overall} summarizes
the results of our performance evaluation
across the entire benchmark suite.
The summary statistics for this
evaluation are tabulated in
figure~\ref{fig:summary-table}.
The worst overhead incurred
by gradual typing with \tool
is a slowdown of @stats:worst-case-opt .
Only @stats:percent-baseline-within-worst-case-opt of benchmark configurations without
contract verification are within this slowdown factor,
while the largest overhead exceeds @stats:worst-case-baseline overhead.

@;

Take the \textsc{sieve} benchmark as an example.
The baseline performance begins at
$50\%$, meaning two of the four
configurations are within a $1\times$ slowdown
of the fully-untyped configuration.
From figure~\ref{fig:lattices}, these are the fully-untyped
configuration itself and the fully-typed configuration.
The one-time increase in the CDF shows the configuration
that has @stats:sieve-small-overhead overhead.
We never see the CDF reach $100\%$ since
this would occur at @stats:sieve-large-overhead ,
beyond the x-axis's range.
By contrast, the CDF for \tool
steeply rises to $100\%$.
This corresponds to no overhead at all.
The visible area of orange is roughly proportional
to the performance improvement of \tool
over Typed Racket.

@;

While \tool makes gains across all benchmarks,
some speed-ups are more noticeable than others.
\textsc{morsecode} has a maximum overhead of
about @stats:morsecode-max-overhead ---an amount that may already
be acceptable to developers.
Here, contract verification yields modest,
but potentially useful gains.
However,
the performance improvements of \tool
become more significant
in benchmarks exhibiting pathological performance
degradations like \textsc{synth} and \textsc{zombie}.
The mean overhead of \textsc{zombie} is @stats:zombie-mean-overhead ,
a slowdown that would likely make
\textsc{zombie},
a video game,
completely unusable.
In this case, sound gradual typing without \tool is infeasible.

@;

There are no pathological cases and any path through the lattice of configurations
from untyped to typed programs exhibit at most @stats:worst-case-opt slowdown.
