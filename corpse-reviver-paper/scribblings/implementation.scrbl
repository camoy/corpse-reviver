#lang scribble/acmart @acmsmall

@title{Implementation}

We implement \tool as a tool for Typed Racket,
which works as a pipeline
taking a mixed-typed source program to optimized bytecode,
can be broken down into three broad phases:
extraction,
verification,
and optimization.

\begin{figure*}
  \begin{subfigure}{0.45\textwidth}
    \includegraphics[width=\textwidth]{figs/ut-require-ty.ps}
    \caption{An untyped {\tt main} requiring a typed {\tt streams}.}
    \label{fig:ut-require-ty}
  \end{subfigure}
  \hspace{0.5em}
  ~
  \begin{subfigure}{0.45\textwidth}
    \includegraphics[width=\textwidth]{figs/ty-require-ut.ps}
    \caption{A typed {\tt main} requiring an untyped {\tt streams}.}
    \label{fig:ty-require-ut}
  \end{subfigure}
  \caption{\tool's contract extraction procedure on the two mixed-typed configurations of {\sc sieve}. Orange ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key3.ps}}}$) represents a typed module, lavender ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key4.ps}}}$) represents an untyped module, blue ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key2.ps}}}$) indicates contracted exports, and red ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key5.ps}}}$) shows an import that bypasses contracts.}
  \Description[Two scenarios: an untyped main module and type streams
  module, and a typed main module and untyped streams module.]{Two
  scenarios: an untyped main module and type streams module, and a
  typed main module and untyped streams module.}
  \label{fig:diagram}
\end{figure*}


@section{Extraction}
\label{sec:extraction}

Typed Racket is Racket's sound gradually-typed
sister language.
Operationally,
it typechecks a fully-expanded program
and outputs untyped Racket syntax
that can be compiled normally.
To ensure soundness,
contracts are inserted at the boundary
between untyped and typed modules.
Problematically,
contract verification
after a program has been fully expanded
is infeasible.
Racket's contract system
is not a privileged part of the language,
but is implemented as a normal library.
As a consequence,
contract forms are expanded into
primitive checks.
Such a low-level representation is not suitable for verification.

We instead intercept contracts generated inside of Typed Racket,
before expansion occurs,
and explicitly attach those contracts to an
erased variant of the typed module.\footnote{
  In actuality,
  types are not syntactically erased,
  but effectively disabled
  using the Typed Racket \texttt{no-check} language.
  These examples are simplified for clarity.
}
Concretely, this transforms
figure~\ref{fig:typed-sieve-streams-code} into
a configuration similar to figure~\ref{fig:untyped-sieve-code}.
Here, implicit contracts attached by Typed Racket
are made explicit in the syntax,
where they can manifest in two different ways,
corresponding to the two different kinds of mixed-type interaction
that must be monitored.

The first situation occurs when an untyped component
calls a typed function.
Imagine if an untyped {\tt main} module
imported the typed {\tt streams} module
as in figure~\ref{fig:ut-require-ty}.
Here, {\tt main} could call {\tt stream-unfold}
with an argument that is not a stream,
an error that must be guarded against at run-time.
Generally, if a typed module is used by an untyped one,
all function arguments must be checked against
their type annotations at run-time.
The converted module in figure~\ref{fig:untyped-sieve-code}
makes these checks explicit.
It protects itself from untyped clients
by exporting its bindings with a contract
via the \texttt{contract-out} form.

The second scenario occurs when a typed module calls an untyped function.
Consider a typed {\tt main} module requiring an untyped {\tt streams} module
as in figure~\ref{fig:ty-require-ut}.
Now a call to {\tt stream-unfold} must check its return value
instead of its argument.
Type annotations are associated with the imported library
via the \texttt{require/typed} form,
and values returning from the untyped module are checked
against this annotation.
To make this explicit,
the converted module
defines a submodule
that attaches contracts to the imported library.
The typed client only interacts with the untyped
library through this proxy module.

The inserted contracts remain unoptimized.
In figure~\ref{fig:untyped-sieve-code},
contracts on the domain of the provided functions are retained
even though they could never be violated at run-time.
Type soundness permits eliminating contracts in every position
where a typed component is responsible.
This would allow us to safely
eliminate contracts in every
positive position in figure~\ref{fig:untyped-sieve-code},
and every negative position in
the dual scenario.
These contracts are kept as-is because
contract verification thrives on more information,
not less.
The presence of more contracts helps verification
by further refining symbolic values.

The final complication is handling library dependencies.
If a module relies on a large external library,
we do not want to analyze its source.
This would be prohibitively time consuming.
Instead, a programmer can mark imports with
an opaque require that \tool
handles specially.
There is no difference between a normal import
and an opaque one at run-time,
but it statically informs the contract verifier
that the dependency should not be analyzed.
During verification,
any values from opaque modules
are treated as entirely unknown.

@section{Verification}

We apply prior work on contract verification using higher-order
symbolic execution to confirm Typed Racket
generated contracts are respected~\cite{local:Nguyen2018Soft}.
%
Although symbolic execution is traditionally used for bug-finding instead of
verification, due to the lack of a termination guarantee,
it can be turned into a verifier by applying well-studied methods
for systematically deriving sound, finite abstractions of an operational
semantics~\cite{dvanhorn:VanHorn2010Abstracting,local:darais2017adi}.
%
Verification of a function {\tt f}, potentially wrapped in a contract, proceeds by
applying a symbolic function to {\tt f}, effectively putting it in an unknown context
exhibiting arbitrary interactions.
%
Soundness of symbolic execution guarantees that the absence of blame on {\tt f}
in the abstraction implies that no concrete interaction with {\tt f}
can blame the function.
\citet{local:Nguyen2018Soft} develop a contract verifier
for Racket called {\sc scv}
that we build upon in \tool.

Both type checking and symbolic execution
predict run-time behaviors of programs.
Correspondingly,
Typed Racket and
\textsc{scv}
are accompanied by soundness theorems.
In the case of Typed Racket,
soundness states that well-typed programs cannot be blamed
at run-time.
Similarly,
\textsc{scv}'s
soundness result states that
a verified module cannot be blamed
at run-time.
Typed Racket's theorem is limited to typed modules,
while \textsc{scv}'s theorem applies to any module under verification.
Therefore, the contract extraction procedure
of section~\ref{sec:extraction}
permits {\sc scv} to reason about both typed and untyped modules.

Analysis of both typed and untyped modules
is necessary to achieve any performance gains
beyond the contract optimizations Typed Racket can already perform.
Type soundness already allows the elimination of
positive contract positions in a typed module.
If {\sc scv} was only to analyze a typed module in isolation,
the best possible result would be to match what
Typed Racket already knows.
Any advantage for contract verification
can only arise from reasoning about
untyped modules,
where the type system has no knowledge.

%Therefore,
%even though contract verification is a modular analysis,
%our optimizations are not.
%To eliminate negative contract positions
%in a typed module,
%we must inspect all untyped use-sites.
%This necessitates
%analysis of all untyped modules in a configuration.
%Concretely,
%importing a typed module that was optimized by \tool
%from an untyped module that was not present during analysis
%is unsound.

Despite the need to analyze both typed and untyped modules,
this does not imply a whole-program assumption.
To the contrary,
both contract verification and our optimization procedure
are modular.
Central to the modularity is the concept of \emph{blame} from higher-order
contracts~\cite{dvanhorn:Findler2002Contracts}.
Blame allows the analysis to
pinpoint which module is the source of a contract failure, and thus to
partition modules by whether they potentially need contracts to ensure
soundness. Without blame, the modularity required would be impossible.
Consequently, our optimizations only bypass
contracts that blame the target module.
Fewer modules available for analysis mean only a lose
in optimization opportunity, never soundness.

The modular nature of the underlying contract verifier enables our
analysis to be \emph{incremental}. Specifically, once a program has
been optimized, changes to the internals of a module require
re-analyzing only that module---the others do not need to be
re-examined. This makes our approach suitable for application to large
code bases when a non-incremental analysis that requires access to the
whole program would be prohibitively expensive on an on-going basis.

@section{Optimization}
\label{sec:erasure}

When all contracts are verified,
such as each configuration of the \textsc{sieve} benchmark,
we may safely bypass contracts that blame either of
the two modules.
For the configuration in figure~\ref{fig:ut-require-ty},
this amounts to modifying
how the untyped code requires the typed code.
Bindings that are always used safely can bypass contract checking,
while potentially unsafe uses will be imported with contracts as normal.
A similar process holds for the configuration in
figure~\ref{fig:ty-require-ut}.

However,
not all contracts may be statically verified,
in which case verification yields a list of
contract positions
that could potentially be blamed at run-time.
This may be due to
a violation that could actually manifest in a concrete execution,
or could be caused by the inherent approximation in
any non-trivial static analysis.
Contract verification is not all-or-nothing.
Failure to verify does not mean all contracts must be kept,
but only those which may incur blame.
In fact,
many configurations in our benchmark suite
contain contracts that cannot be verified.
Failure to verify every contract in a configuration
does not prevent us from eliminating
{\it almost} all of them.
As section~\ref{sec:eval} demonstrates,
this is sufficient to gain substantial performance improvements.

From {\sc scv}'s list of contract positions that may be violated
at run-time,
we must determine which contracts to retain.
Typed Racket generates auxiliary contract definitions
that are used and shared among
the contracts that are ultimately attached to
a module's exports.
To determine which contracts may be eliminated,
we construct a directed graph of contract dependencies.
Any contract that is reachable from
one that cannot be verified
must be kept.
Partially verified contracts may be residualized.
For example,
if a function contract's co-domain
cannot be verified,
but its domain can,
then we may safely remove the check for the domain.
This amounts to replacing the domain contract with \texttt{any/c},
the contract that does no checking.

Our optimization procedure also takes advantage of
the knowledge that typed modules
are proven type-safe by Typed Racket.
In particular, we ignore any result from
the contract verifier that blames a
typed module since this must be a false positive.

After optimization,
\tool
outputs bytecode.
There are two reasons for this choice:
one pragmatic, and one technical.
The pragmatic reason is so the tool can be used as a drop-in replacement
for Racket's \texttt{make} command.
A developer can replace a single line in
their build script
and get an optimized program.
The technical reason
is to preserve the lexical information contained in the syntax.
Contract definitions can,
for example,
rely on unexported identifiers from other modules.
Writing optimized source code to a file
would lose this critical information.
