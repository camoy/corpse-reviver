#lang scribble/acmart @acmsmall

@title{A path to revival}

The landmark study on the empirical performance of run-time
enforcement of sound gradual types by~\citet{local:takikawa2016} paints
a negative picture, justifiably concluding that ``in the context
of current implementation technology, sound gradual typing is dead.''
The thesis is supported by benchmarking results over an exhaustive
enumeration of all possible gradual typed configurations of a
program.  The results demonstrate the cost of enforcing soundness is
overwhelming and that for almost all benchmarks @emph{there is no path
  from a fully-untyped program to a fully-typed program with
  acceptable performance overhead}, casting doubt on the vision of
gradual typing as a means for incrementally fortifying programs with
the benefit of static types.

In the ensuing years, researchers sought to improve the
implementation technology of enforcement so as to drive down
run-time cost.  Many of these enhancements target pathological
cases identified in by \citet{local:takikawa2016}.
None, however, achieve across the board acceptability numbers.

In this paper, we have taken a different tack.
Rather than improving run-time enforcement mechanisms, we seek to remove
their use when possible.  Using lightweight formal methods based
on contract verification, we find that type abstractions enforced
on untyped code can be effectively validated statically and thereby
eliminated.  The results are promising---across the benchmark suite,
the average overhead is acceptable in all cases, and even the worst
case performance is acceptable in all but a few cases.
There are no pathological cases and any path through the lattice of configurations
from untyped to typed programs exhibit at most 1.5\times\ slowdown.
All of this is achieved @emph{without} improving run-time
mechanisms, which are orthogonal and can offer complementary benefits.

Traditional perspectives on gradual typing suggest that statically
reasoning about code should only be the purview of the typed portion
of a program.  This paper shows that there is considerable
promise in statically reasoning about the {\it untyped portion} in a gradually-typed
program, particularly in the context of the invariants generated
by typed components.  While the untyped portion of code may not
adhere to a particular static type system, type abstractions may still
be validated by other means.  Contract verification appears to be a
fruitful approach.

@section[#:style 'unnumbered]{On benchmark selection}

We began with the hypothesis that in many gradually-typed programs,
the untyped as well as the typed can be shown not to have run-time type errors,
and our evaluation shows this hypothesis led us to an
implementation that is highly effective on the most widely-used suite
of gradual typing benchmarks.

However, perhaps we should be unsurprised by this outcome. After all,
the benchmarks we use were constructed by first taking programs that
are fully-typeable, and then removing some of the types. Thus every
program is (nearly) typeable by construction!\footnote{The untyped portions of
  the program are not simply the typed versions with the types
  deleted, but the original untyped programs, and thus may and do have
some differences from the typed versions.} Moreover, this is both the
consistent approach taken to develop this benchmark
suite~\cite{local:greenman2019} and thus used in several other
gradual typing evaluations~\cite{local:bauman2017mostly,local:feltey2018collapsible}, but is also
the standard approach to generate benchmarks for @emph{other}
gradual type
systems~\cite{local:vitousek2019optimizing,local:vitousek2017big,local:greenman2018spectrum,local:kuhlenschmidt2019toward,local:muehlboeck2017sound,DBLP:journals/pacmpl/RichardsAT17}:
none
include benchmarks that are not known to be typeable.

In our case, the threat to the validity of our evaluation is somewhat
mitigated by the substantial @emph{differences} between \textsc{scv}'s analysis
and Typed Racket's---that some module is in-principle typeable with
Typed Racket implies no particular result for \textsc{scv}. However, this is
clearly a potential threat to the validity of our results, and to the
results of gradual typing optimization research in general.

We suggest that future gradual typing benchmark developers, and
gradual type system implementors and evaluators, consider programs
beyond the easily-typed. We need benchmarks that cannot be 100\%
verified, even in principle, because they contain potential runtime
errors reachable with certain inputs. This is likely to be the case in
every realistic system, and should be considered in research and
evaluation.
