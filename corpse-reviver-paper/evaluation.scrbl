#lang scribble/acmart @acmsmall

@(require "private/figure.rkt")

@title{Evaluation}

We claim that
contract verification of gradually-typed programs
can eliminate effectively all the overhead
of enforcing higher-order soundness.
To evaluate this claim,
we compare the latest version of Typed Racket
against \tool.
The benchmark suite used
is standard
for assessing the feasibility
of gradual typing.
The artifact for
\tool
is available
along with instructions for
reproducing these results.\footnote{
  \url{https://github.com/scv-cr/scv-cr} (Repository is anonymized)
}

@section{Benchmark programs}

We use the
benchmark suite first developed by
\citet{local:takikawa2016}
and
expanded by
\citet{local:greenman2019}.
Our evaluation pits \tool
against Typed Racket on
12 of the 20 programs in the
benchmark suite.
The 8 programs that remain
use object-oriented features
that are not supported by {\sc scv}.
We made other minor changes to the programs
to avoid other features not supported by {\sc scv}.
For example,
keyword arguments were changed to positional arguments,
and parameters\footnote{
  Here ``parameter'' refers to Racket's dynamic binding
  mechanism, not in the sense of a formal argument.}
were changed to boxes.
All measurements,
including the baseline performance,
were made with respect to this modified suite.
It exhibits the same performance characteristics
as the unmodified suite.

Each benchmark consists of a number of modules
with both a typed and untyped variant.
This results in $2^n$
possible configurations for a program
with $n$ modules.

@section{Two benchmarks in detail}

The \textsc{suffixtree} benchmark
originates from a library for computing
Ukkonen's suffix tree algorithm.
The primary source of performance
overhead is due to a contract boundary
between the library of data structures
and the functions for manipulating those
structures.
For the configuration in which all modules are untyped
except for the \texttt{data} module,
the primary overhead is due to a single struct accessor.
Here is the definition for a label structure:

\begin{Verbatim}[fontsize=\small,xleftmargin=1.5em]
(struct label
  ([datum : (Vectorof (U Char Symbol))]
   [i : Natural]
   [j : Natural])
  #:mutable)
\end{Verbatim}

This definition automatically generates an
accessor function for the \texttt{datum} field.
If this function is exported,
it is protected with the following contract.

\begin{Verbatim}[fontsize=\small,xleftmargin=1.5em]
(-> label? (vectorof (or/c Symbol Char)))
\end{Verbatim}

According to Racket's contract profiler~\cite{local:andersen2018},
this contract constitutes
approximately $70\%$ of the running time of this configuration.
%
Because \textsc{scv} verifies that all calls to this accessor
respect the contract's negative position, {\tt label?},
the accessor can be exported as-is without a wrapper.

The \textsc{zombie} benchmark
was ported to Typed Racket from the original benchmark suite
for \textsc{scv}.
Initially,
the most significant overhead was due to
the accumulation of higher-order wrappers
from frequent boundary crossings.
This issue was addressed by improvements
made to Racket's contract system~\cite{local:feltey2018collapsible}.
Despite these substantial gains,
figure~\ref{fig:summary-table} indicates that
\textsc{zombie} still has a mean overhead of $28.2\times$.
In the case of a fully-untyped configuration
except for a \texttt{zombie} module,
the overhead is mostly due to a contract attached to
\texttt{world-on-tick}.
This function is protected by a
\texttt{(-> world/c (-> world/c))}
contract where \texttt{world/c}
is defined as follows:

\begin{Verbatim}[fontsize=\small,xleftmargin=1.5em]
(recursive-contract
 (-> symbol?
  (or/c
    (cons/c 'stop-when (-> boolean?))
    (cons/c 'to-draw (-> image?))
    (cons/c 'on-tick (-> world/c)
    (cons/c
      'on-mouse
      (-> real? real? string? world/c))))))
\end{Verbatim}

The {\tt world/c} contract enforces that each world ``object'' be a function that accepts
``messages'' as symbols, and returns a corresponding ``method'' paired with
the same message that it receives.
%
This seemingly redundant encoding was introduced in
the {\sc zombie} variant used in the gradual typing benchmarks,
and differs from the original encoding using dependent
contracts~\cite{local:Nguyen2014Soft}.
Typed Racket does not generate dependent contracts,
and a simple intersection type would generate a {\tt case->} contract
whose cases could be first-order indistinguishable,
violating a general requirement of {\tt case->} contracts.
This change introduces a minor challenge to the original implementation
of \textsc{scv}, because it requires that at most one higher-order disjunct is
provided to {\tt or/c}.

To verify the modified version of {\sc zombie}, we generalize \textsc{scv}
to accept more {\tt or/c} contracts, matching Racket's semantics more closely.
Instead of requiring no more than one higher-order disjunct,
we accept any pair of contracts as disjuncts,
as long as the disjuncts are first-order distinguishable at each monitoring site.
When monitoring a value against a disjunctive contract,
it is first checked against the first-order parts of each disjunct.
Execution proceeds with the first-order satisfied disjunct if there is no ambiguity,
and raises an error otherwise.
In this case, each {\tt cons/c} contract has a tag as its first component,
which is easily distinguished from one another.

@section{Experimental setup}

The experiment was run on a Linux machine
with an Intel Xeon E5 processor
running at $2.60$ GHz
with $63$ GB of RAM.
All measurements were taken with Racket $7.4$.
This release includes
improvements made by~\citet{local:feltey2018}
to the run-time representation
and performance of contracts.

For each benchmark,
all $2^n$ configurations were measured
except for \textsc{gregor}.
Due to the large number of possible configurations
in this benchmark,
we instead took $10$ random samples of
$130$ configurations each.
This resulted in a total of
$1,300$ random configurations.
A random sample of
configurations can
approximate
the true performance of an exponentially-large
number of configurations~\cite{local:greenman2019,local:greenman2017}.
Each configuration was run for $10$ iterations
with the mean value used in
the lattices of figure~\ref{fig:lattices}.
When sampling,
we used the same configurations
for the baseline and \tool measurements
and did not resample.

@fig:overhead-grid

@fig:exact-grid

@;\begin{figure*}
@;  {\renewcommand{\arraystretch}{5}
@;  \begin{tabular}{c@{\hskip 3em} c}
@;    \includegraphics[width=0.45\textwidth]{figs/fsm-overhead.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/gregor-overhead.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/kcfa-overhead.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/lnm-overhead.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/morsecode-overhead.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/sieve-overhead.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/snake-overhead.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/suffixtree-overhead.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/synth-overhead.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/tetris-overhead.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/zombie-overhead.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/zordoz-overhead.ps}
@;  \end{tabular}
@;  }
@;  %\caption{The purple (\scalerel*{\protect\includegraphics{figs/key0.ps}}{B}) curve is Typed Racket and the orange (\scalerel*{\protect\includegraphics{figs/key1.ps}}{B}) curve is \tool. Overhead of gradual typing for each benchmark individually. Each point $(x, y)$ indicates an $x$-factor slowdown over the fully untyped configuration for $y\%$ of configurations. The dashed lines between $1$ and $2$ occur at increments of $0.2$ and between $2$ and $10$ at increments of $2$.}
@;  \caption{Overhead of gradual typing for each benchmark individually. The purple ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key0.ps}}}$) curve is Typed Racket and the orange ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key1.ps}}}$) curve is \tool. Each point $(x, y)$ indicates an $x$-factor slowdown over the fully-untyped configuration for $y\%$ of configurations. The dashed lines between $1$ and $2$ occur at increments of $0.2$ and between $2$ and $10$ at increments of $2$.}
@;  \label{fig:overhead}
@;\end{figure*}
@;
@;\begin{figure*}
@;  {\renewcommand{\arraystretch}{5}
@;  \begin{tabular}{c@{\hskip 3em} c}
@;    \includegraphics[width=0.45\textwidth]{figs/fsm-exact.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/gregor-exact.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/kcfa-exact.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/lnm-exact.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/morsecode-exact.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/sieve-exact.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/snake-exact.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/suffixtree-exact.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/synth-exact.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/tetris-exact.ps} \\
@;    \includegraphics[width=0.45\textwidth]{figs/zombie-exact.ps} &
@;    \includegraphics[width=0.45\textwidth]{figs/zordoz-exact.ps}
@;  \end{tabular}
@;  }
@;  \caption{Exact time it takes each configuration to execute. The purple ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key0.ps}}}$) curve is Typed Racket and the orange ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key1.ps}}}$) curve is \tool. The x-axis is binned by the number of typed modules in a configuration. The y-axis is time to execute in seconds.}
@;  \label{fig:exact}
@;\end{figure*}
@;
@;\pagebreak

@section{Results}

\begin{wrapfigure}{r}{0.55\textwidth}
\definecolor{rktpink}{RGB}{255, 192, 203}
\definecolor{rktpalegreen}{RGB}{152, 251, 152}
\input{figs/summary.tex}
  \caption{Maximum and mean overhead for Racket $7.4$ and \tool for each benchmark. Red indicates a slowdown $\geq 3\times$ while green indicates a slowdown $\leq 1.25\times$.}
  \label{fig:summary-table}
\end{wrapfigure}

Figure~\ref{fig:overall} summarizes
the results of our performance evaluation
across the entire benchmark suite.
The summary statistics for this
evaluation are tabulated in
figure~\ref{fig:summary-table}.
The worst overhead incurred
by gradual typing with \tool
is a slowdown of $1.5\times$.
Only $29\%$ of benchmark configurations without
contract verification are within this slowdown factor,
while the largest overhead exceeds $61\times$ overhead.

Figure~\ref{fig:overhead} shows the overhead plots for each
benchmark.
An overhead plot represents the performance feasibility of a
gradual type system for a particular program.
The log-scaled x-axis indicates performance overhead
as a factor of the benchmark's fully-untyped configuration.
The y-axis indicates the percent
of all configurations
that are within this slowdown factor.
Both the unoptimized performance in purple,
and the performance with \tool in orange,
are plotted on the same axes.

Take the \textsc{sieve} benchmark as an example.
The baseline performance begins at
$50\%$, meaning two of the four
configurations are within a $1\times$ slowdown
of the fully-untyped configuration.
From figure~\ref{fig:lattices}, these are the fully-untyped
configuration itself and the fully-typed configuration.
The one-time increase in the CDF shows the configuration
that has $4.7\times$ overhead.
We never see the CDF reach $100\%$ since
this would occur at $17.3\times$,
beyond the x-axis's range.
By contrast, the CDF for \tool
steeply rises to $100\%$.
This corresponds to no overhead at all.
The visible area of orange is roughly proportional
to the performance improvement of \tool
over Typed Racket.

While \tool makes gains across all benchmarks,
some speed-ups are more noticeable than others.
\textsc{morsecode} has a maximum overhead of
about $1.9\times$---an amount that may already
be acceptable to developers.\footnote{
  This judgment is domain-specific. For some applications, $2\times$ overhead may be unacceptable, while in others a $10\times$ slowdown may be acceptable. There is no magic constant. For their Sorbet system, Stripe allows only a $7\%$ slowdown before paging developers.
}
Here, contract verification yields modest,
but potentially useful gains.
However,
the performance improvements of \tool
become more significant
in benchmarks exhibiting pathological performance
degradations like \textsc{synth} and \textsc{zombie}.
The mean overhead of \textsc{zombie} is $28.2\times$,
a slowdown that would likely make
\textsc{zombie},
a video game,
completely unusable.
In this case, sound gradual typing without \tool is infeasible.

Figure~\ref{fig:exact} displays exact run-time plots
that show all this information
in granular detail.
Every point is a single execution of a configuration.
The x-axis is binned according to how many typed modules
are in a configuration and points are jittered within this bin
for clarity.
The y-axis is the exact run-time of the configuration
in seconds.
Rows of $10$ points are frequently visible in these plots,
and are typically points that correspond to different
iterations of the same configuration.

@section{Limitations and future work}

Typed Racket performs two kinds of optimization:
it eliminates contracts that type soundness proves cannot be violated at run-time,
and it specializes program behavior based on static type information.

Optimizations of the first sort are the kind that
\tool can perform,
but Typed Racket is more sophisticated in its optimizations
and can often out-perform \tool fully-typed configurations
where overhead due to boundary crossing is not a challenge.
See the fully-typed configurations
of \textsc{fsm} and \textsc{morsecode}
in figure~\ref{fig:exact}
as an example.
In \textsc{fsm}
there is a loop that runs several million times.
The loop extracts the head and tail of a list,
incurring a tag check that the argument is a list.
Since Typed Racket can prove the argument is a list,
this check is eliminated by rewriting {\tt first}
to {\tt car}.
This optimization is the main source of performance
improvement in this configuration.
\tool does not perform this optimization
as it only bypasses boundary contracts between
user-defined modules.

\tool does not perform the second form of optimization,
behavior specialization,
at all.
Typed Racket can rewrite {\tt +},
an operation that performs a tag check on its arguments,
as {\tt unsafe-fx+}, which does not.

Our tool is conservative
in that it does not rely on specific facts derived from type-checking.
The optimization only takes advantage of the knowledge that
typed module are blame-free.
We do not perform any optimizations beyond bypassing contracts.
In principle all of Typed Racket's optimizations could be
safely performed by \tool and this discrepancy would disappear.
Integrating facts derived by the type-checker
and the contract verifier
for additional elimination and optimization is future work.

Our tool could also benefit from
refinements to the existing implementation.
For example,
the only source of slowdown in the
{\sc zombie}
benchmark
is the inability of the underlying analysis
to prove that a specific symbolic value known to be a number,
which can include complex numbers in Racket,
is real.
Increasing the precision
of {\sc scv}'s analysis,
by encoding more language-specific knowledge,
could further close the performance gap.

Despite all this,
our evaluation demonstrates that
contract verification can eliminate the bulk of
overhead due to gradual typing.
It is conceivable that minor refinements to the
method could close the gap altogether.
