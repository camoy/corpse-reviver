#lang corpse-reviver-paper

@title{Static verification to avoid dynamic costs}

Gradual
typing@~cite[siek-2006 tobin-hochstadt-2006]
has become a popular approach to
integrate static types into existing
dynamically-typed programming
languages@~cite[tobin-hochstadt-2008 typescript-2012 chaudhuri-2017 sorbet-2019].
It promises to combine the benefits of compile-time static checking
such as
optimization,
tooling,
and enforcement of invariants,
while accommodating the existing idioms of popular languages
such as
Python,
JavaScript,
and others.

The technology enabling this combination to be safe
is @italic{higher-order contracts}@~cite[findler-2002],
which allow the typed portion of a program
to protect its invariants,
even when higher-order values
such as
functions,
objects,
or mutable values
flow back and forth between components.
Contracts also support @italic{blame},
that specifies which component failed
when an invariant is violated.
In sound gradually-typed languages,
when one of the generated contracts fails,
blame always lies with an untyped component.

Unfortunately,
dynamic enforcement of types comes at a cost,
since run-time checks must be executed whenever
values flow between typed and untyped components.
Furthermore,
when a higher-order value crosses a type boundary,
the value must be wrapped.
This imposes overhead from
wrapper allocation,
indirection,
and
checking.

Recent large-scale studies,
as well as significant anecdotal evidence,
have found this cost to be unacceptably
high@~cite[takikawa-2016 greenman-2019].
Some real programs,
when migrated in a specific way,
exhibit slowdowns that likely render them unusable
for their actual purpose.
Even less-problematic examples
may exhibit significant slowdown.
Research implementations designed for speed
often perform much better,
but still suffer an up to
@format-overhead[8] slowdown@~cite[kuhlenschmidt-2019].

Faced with this obstacle,
many systems abandon some
or all of the semantic advantages of gradual typing,
in several cases giving up entirely on
run-time enforcement of types@~cite[greenman-2018].
TypeScript@~cite[typescript-2012],
Flow@~cite[chaudhuri-2017],
MyPy@~cite[lehtosalo-2017],
and others omit dynamic checks,
making their type systems unsound.
Others,
such as
Grace@~cite[black-2012],
Sorbet@~cite[sorbet-2019],
and Reticulated Python@~cite[vitousek-2014],
keep some dynamic checking,
but give up the full soundness guarantee
offered by gradual typing.
Yet other systems,
such as
Safe TypeScript@~cite[rastogi-2015],
Nom@~cite[muehlboeck-2017],
Thorn@~cite[wrigstad-2010],
and Dart@~cite[dart-2011]
limit interoperability between
typed
and untyped code
to avoid some expensive checks.

We offer a new approach
to the dilemma of gradual type enforcement
without giving up either
the semantic benefits of soundness
or efficient execution.
@italic{
Our key idea
is that dynamic contracts
are statically useful
}.
Our tool,
@|scv-cr|,
statically verifies contracts generated by Typed Racket,
an existing gradually-typed language,
eliminating those that cannot fail at run time.
These contracts generate significant,
useful information
which can be used to reason about the static behavior
of all code,
even in the absence of static types.
In particular,
contracts characterize the allowable interactions between
typed and untyped code,
which can be used to validate
that untyped code
respects the type abstractions
of its typed counterparts.

Building on a sound and precise
higher-order symbolic execution system
for a large subset of Racket@~cite[nguyen-2018],
@scv-cr eliminates almost all of the contracts
generated by Typed Racket
across a dozen pre-existing benchmarks@~cite[greenman-2019].
As shown in @figure-ref{fig:overhead-summary},
after our optimizations,
almost no performance overhead remains,
despite the presence of catastrophic overhead
even in some simple benchmarks we study.
In short,
this work focuses on eliminating checks
that are not going to fail,
rather than worrying about their expense,
and we show that this direction holds
significant promise
for making gradual typing performant.

@figure["fig:overhead-summary"]{
@elem{
Overhead of gradual typing over the whole benchmark suite.
The purple (@|fig:purple-key|) curve is Typed Racket
and the orange (@|fig:orange-key|) curve is @|scv-cr|.
The log-scaled x-axis indicates slowdown factor
compared against the
fully-untyped configuration,
while the y-axis indicates the
percent of configurations incurring that slowdown.
Each benchmark is allocated
an equal proportion of the y-axis.
Higher is better.
}
@fig:overhead-summary
}

Furthermore,
by leveraging the notion of
@italic{blame}@~cite[findler-2002],
our analysis
and optimization
is fully modular.
Any single module can be analyzed in isolation,
and potential failures from one module,
or even one contract in a module,
do not prevent the optimization
of other contracts in the module.

The standard soundness result for typed  programming is often
sloganized as ``well-typed programs don't go @italic{wrong}.''
It has been adapted
in the setting of gradually-typed programming
to ``well-typed modules can't be @italic{blamed}''@~cite[tobin-hochstadt-2006].
Essentially,
things can go wrong at run time,
but it is always the untyped code's fault.
This is a lovely property,
but one that perhaps paints untyped code
too broadly as unreasonable.
Research on gradually-typed languages
usually treats untyped modules as code for which
all bets are off.
If we can't statically know anything about the untyped code,
then optimizations must focus on
the mechanisms enforcing the disciplines
of the typed code
within the untyped code,
leading to a wide variety of enforcement strategies.

Our work begins instead from the hypothesis that
``untyped modules can be blamed, @italic{but usually aren't}.''
In other words,
untyped code may not follow the static discipline
of a given type system,
but it often does follow that discipline dynamically.
Moreover,
the static requirements,
formulated as dynamic contracts,
can be validated against untyped code.
What is needed is a verification method
able to closely model dynamic idioms of untyped languages,
for which we find higher-order symbolic execution a good fit.

@nested[#:style 'inset]{
@italic{Contributions.}
This paper contributes:

@itemlist[
@item{
the idea that dynamic contracts are statically useful for
optimizing gradually-typed programs
by verifying contracts against
the untyped portions of a program;
}

@item{
a technique for reducing the problem of optimizing
a gradually-typed program
into the problem of modular contract verification,
formalized in a simple gradually-typed calculus;
}

@item{
a tool that implements these ideas,
integrating Typed Racket
and an existing contract verification system
based on higher-order symbolic execution;
}

@item{
and an evaluation demonstrating
the effectiveness of our approach on a variety of programs
from the canonical gradual typing benchmark suite,
omitting only those beyond the scope
of the symbolic execution engine we employ.
}
]
}

@margin-note{
  The percent is normalized such that all benchmarks are weighed equally,
  even though some may contain many more configurations than others.
}

The overall performance of our system
is visualized in the cumulative distribution function (CDF) plot
in @figure-ref{fig:overhead-summary}.
This plot follows the conventions of @citet[takikawa-2016],
and represents the normalized percentage of configurations
(on the y-axis)
that have less than the given slowdown
(on the x-axis, log-scale).

For example,
Typed Racket 7.7 runs @stat:%-2x-baseline of benchmark configurations
with less than @format-overhead[2] slowdown.
With @|scv-cr|,
@format-percent[0.95] of benchmark configurations
have less than @stat:95%-quantile-opt slowdown
compared to the fully-untyped configuration.
As this plot makes clear,
@scv-cr reduces overhead to nearly zero
in almost all cases,
and completely eliminates all overhead
above @|stat:max-opt|.

In the remainder of this paper,
we describe our approach,
why it works well on gradual typing,
and provide an extensive evaluation.
We begin with an example-driven overview
of how contract verification can
eliminate gradual typing dynamic checks
(TODO).
Next,
we formalize our ideas
(TODO)
in a simple,
gradually-typed language of modules and functions,
which compiles to the language of contracts
considered by @citet[nguyen-2018],
and show how the soundness of our optimizer
is a corrollary of the soundness result
for their symbolic executor.
Then,
we describe the implementation
(TODO),
including integration with Typed Racket,
use of an existing symbolic execution engine,
and subsequent optimization.
We evaluate our tool
(TODO)
on a dozen pre-existing benchmarks
drawn from @citet[greenman-2019],
elaborating on the summary given in
@figure-ref{fig:overhead-summary}.
Finally,
we conclude
with a perspective on how
our results point to
potential improvments
in gradual typing evaluation.
