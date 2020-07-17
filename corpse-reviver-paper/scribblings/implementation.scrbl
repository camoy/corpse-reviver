#lang corpse-reviver-paper

@title{Implementation}

We implemented @scv-cr as a tool for Typed Racket,
that takes a mixed-typed source program as input
and outputs optimized bytecode.
This process
can be broken down into three phases:
extraction,
verification,
and optimization.

@figure["fig:ut-require-ty"]{
@elem{
An untyped @tt{main}
requiring a typed @tt{streams}.
This diagram describes how @scv-cr
optimizes a mixed-typed configuration of @sc{sieve}.
Orange (@|fig:orange-key|) represents a typed module,
lavender (@|fig:lavender-key|) represents an untyped module,
blue (@|fig:blue-key|) indicates contracted exports,
and red (@|fig:red-key|) shows an import that bypasses contracts.
}
@fig:ut-require-ty
}

@figure["fig:ty-require-ut"]{
@elem{
A typed @tt{main}
requiring an untyped @tt{streams}.
Contracts on the untyped module
are introduced
with a proxy submodule.
}
@fig:ty-require-ut
}

@section{Extraction}

Typed Racket is Racket's sound gradually-typed
sister language.
Operationally,
it typechecks a fully macro-expanded program
and outputs untyped Racket syntax
that can be compiled normally.
To ensure soundness,
contracts are inserted at the boundary
between untyped and typed components.
Problematically,
contract verification
after a program has been fully expanded
is infeasible.
Racket's contract system
is not a privileged part of the language,
but is implemented as a normal library.
As such,
contract forms are expanded into
primitive checks.
Such a low-level representation
is not suitable for verification.

@margin-note{
  In actuality,
  types are not syntactically erased,
  but effectively disabled
  using the Typed Racket @tt{no-check} language.
  Our examples omit this detail for clarity.
}

We instead intercept contracts generated inside of Typed Racket,
before expansion occurs,
and explicitly attach those contracts to an
erased variant of the typed module.

Concretely,
this transforms the code from @figure-ref{fig:typed-streams}
into a configuration resembling @figure-ref{fig:untyped-sieve}.
Here,
implicit contracts attached by Typed Racket
are made explicit in the syntax,
where they can manifest in two different ways,
corresponding to the two different kinds of mixed-type interaction
that must be monitored.

The first situation occurs when an untyped component
calls a typed function.
Imagine if an untyped @tt{main} module
imported the typed @tt{streams} module;
see @figure-ref{fig:ut-require-ty}.
Here,
@tt{main} could call @tt{stream-unfold}
with an argument that is not a stream,
an error that must be guarded against at run time.
Generally,
if a typed module is used by an untyped one,
all function arguments must be checked against
their type annotations at run time.
The converted module in @figure-ref{fig:untyped-sieve}
makes these checks explicit---it
protects itself from untyped clients
by exporting its bindings with a contract
via the @tt{contract-out} form.

The second scenario occurs
when a typed module calls an untyped function.
Consider a typed @tt{main} module
requiring an untyped @tt{streams} module;
see @figure-ref{fig:ty-require-ut}.
A call to @tt{stream-unfold} must now check its return value
instead of its argument.
Type annotations are associated with the imported library
via the @tt{require/typed} form,
and values returning from the untyped module are checked
against this annotation.
To make this explicit,
@scv-cr defines a submodule
that attaches contracts to the imported library.
A typed client only interacts with an untyped
library through this proxy module.

Note that the inserted contracts are unoptimized.
In @figure-ref{fig:untyped-sieve},
contracts on the domain of the provided functions are retained
even though they could never be violated at run time.
Type soundness permits eliminating contracts in every position
where a typed component is responsible.
This would allow us to safely
eliminate contracts in every
positive position in @figure-ref{fig:untyped-sieve},
and every negative position in
the dual scenario.
These contracts are kept as-is because
contract verification thrives on more information,
not less.
Thus,
more contracts helps the verifier
by further refining symbolic values.

One final complication is handling library dependencies.
If a module relies on a large external library,
we do not want to analyze its source.
This would be prohibitively time consuming.
Instead,
a programmer can mark imports with
an opaque require that @scv-cr
handles specially.
There is no difference between a normal import
and an opaque one at run time,
but it statically informs the contract verifier
that the dependency should not be analyzed.
During verification,
any values from opaque modules
are treated as entirely unknown.

@section{Verification}

We apply prior work on contract verification using higher-order
symbolic execution to confirm that Typed Racket
generated contracts are respected@~cite[nguyen-2018].

Although symbolic execution is traditionally used
for bug-finding instead of verification,
due to the lack of a termination guarantee,
it can be turned into a verifier by applying
well-studied methods for systematically deriving sound,
finite abstractions of
an operational semantics@~cite[van-horn-2010 darais-2017].

Verification of a function @tt{f},
potentially wrapped in a contract, proceeds by
applying a symbolic function to @tt{f},
effectively putting it in an unknown context
exhibiting arbitrary interactions.

Soundness of symbolic execution
guarantees that the absence of blame on @tt{f}
in the abstraction implies that
no concrete interaction with @tt{f}
can blame the function.
@citet[nguyen-2018] develop a contract verifier
for Racket called @sc{scv}
that we build upon in @|scv-cr|.

Both typechecking and symbolic execution
predict run-time behaviors of programs.
Correspondingly,
Typed Racket and @sc{scv}
are accompanied by soundness theorems.
In the case of Typed Racket,
soundness states that well-typed programs cannot be blamed
at run time.
Similarly,
@sc{scv}'s soundness result states that
a verified module cannot be blamed
at run time.
Typed Racket's theorem is limited to typed modules,
while @sc{scv}'s theorem applies to any module under verification.
Therefore, the contract extraction procedure
of (TODO ref sec:extraction)
permits @sc{scv} to reason about both typed and untyped modules.

Analysis of typed and untyped modules
is necessary to achieve any performance gains
beyond the optimizations that Typed Racket can already perform.
Type soundness already allows the elimination of
positive contract positions in a typed module.
If @sc{scv} was only to analyze a typed module in isolation,
the best possible result would be to match what
Typed Racket already does.
Any advantage for contract verification
can only arise from reasoning about
untyped modules,
where the type system has no knowledge.

Despite the need to analyze both typed and untyped modules,
this does not imply a whole-program assumption.
To the contrary,
both contract verification and our optimization procedure
are modular.
Central to the modularity our approach
is the concept of @italic{blame}
from higher-order contracts@~cite[findler-2002].
Blame allows the analysis to
pinpoint which module is the source of a contract failure, and thus
partitions modules by whether they potentially.
Without blame,
modularity would be impossible.
Consequently,
our optimization only bypasses
contracts that are proven not to blame the target module.
Fewer modules available for analysis mean only a lose
in optimization opportunity,
never soundness.

The modular nature of the underlying contract verifier
also enables our analysis to be @italic{incremental}.
To eliminate contracts at a boundary,
only the two parties involved need to be analyzed---others
need not be examined.
This makes our approach suitable
for application to large code bases
when a non-incremental analysis
that requires access to the whole program
would be prohibitively expensive
on an on-going basis.

@section{Optimization}

When all contracts are verified,
such as each configuration of the @sc{sieve} benchmark,
we may safely bypass contracts that blame either of
the two modules.
For the configurations in
@figure-ref{fig:ut-require-ty} and @figure-ref{fig:ty-require-ut},
this amounts to modifying
how untyped code requires typed code,
and vice versa.
Bindings that are always used safely
can bypass contract checking,
while potentially unsafe uses
will be imported with contracts as normal.

When some contracts fail to verify,
the verifier reports
contract positions
that could be blamed at run time.
This may be due to
a violation that could manifest in a concrete execution,
or due to the inherent approximation in
any non-trivial static analysis.
Contract verification is not all-or-nothing.
Failure to verify does not mean all contracts are kept---only
those which may incur blame.
Failure to verify every contract in a configuration
does not prevent us from eliminating
@italic{almost} all of them.
As (TODO section-ref sec:eval) demonstrates,
this is sufficient to gain substantial performance improvements.

From @sc{scv}'s list of contract positions,
we must determine which contracts to retain.
Typed Racket generates auxiliary contract definitions
that are used and shared among
contracts that are ultimately attached to
a module's exports.
To determine which contracts may be eliminated,
we construct a directed graph of contract dependencies.
Any contract that is reachable from
one that cannot be verified
must be kept.

Our optimization procedure also takes advantage of
the knowledge that typed modules
are proven safe by Typed Racket.
In particular, we ignore any result from
the contract verifier that blames a
typed module since this must be a false positive.
We also ignore any blame from contracts other than Typed Racket's,
such as those coming from Racket's standard library.

After optimization,
@scv-cr outputs bytecode.
There are two reasons for this choice:
one pragmatic,
and one technical.
Pragmatically,
outputting bytecode means @scv-cr can be used
as a drop-in replacement
for Racket's existing @tt{make} command.
A developer can replace a single line in
their build script
and get an optimized program.
The technical reason
is to preserve the lexical information
contained in the source program's syntax.
Contract definitions can,
for example,
rely on unexported identifiers from other modules.
Writing,
for example,
optimized source code to a file
would lose this critical information.
