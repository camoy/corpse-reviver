#lang scribble/acmart @acmsmall

@(require "private/cite.rkt"
          "private/figure.rkt")

@title{Introduction}

Gradual typing~\cite{local:siek2006gradual,dvanhorn:TobinHochstadt2006Interlanguage} has become a popular approach to
integrate static types into existing dynamically-typed programming
languages~\cite{local:ts,local:flow,local:sorbet,dvanhorn:tobin-hochstadt-felleisen-popl08}.
It promises to combine the benefits of compile-time static checking such
as optimization, tooling, and enforcement of invariants, while
accommodating the existing idioms and programs of popular languages
such as Ruby, PHP, JavaScript, and others.

The key technology enabling this combination to be safe is @emph{higher-order
contracts} \cite{dvanhorn:Findler2002Contracts}, which allow the typed portion of
a program to protect its invariants, even when higher-order values such
as functions, objects, or mutable arrays flow back and forth between
components.
Contracts also support @emph{blame}, that
specifies which component failed when an invariant is violated.
In sound gradually-typed languages,
when one of the generated contracts fails,
blame will always lie with an untyped component.

Unfortunately, dynamic enforcement of types comes at a
cost, since run-time checks must be executed whenever
values flow between typed and untyped components. Furthermore, when
a higher-order value crosses a type boundary,
the value must be wrapped.
This imposes overhead from wrapper allocation,
indirection,
and checking.

Recent large-scale studies, as well as significant anecdotal evidence,
have found this cost to be unacceptably high@~local:cite{takikawa2016 greenman2019}.
Some realistic programs, when migrated in a specific way,
can exhibit slowdowns over $20\times$,
likely rendering them unusable for their actual purpose.
Even less-problematic examples often exhibit significant
slowdowns. When research implementations designed for speed are
compared, they often perform much better, but can still suffer
up to $8\times$ slowdown~\cite{local:kuhlenschmidt2019toward}.

Faced with this obstacle, many systems abandon some or all of
the semantic advantages of gradual typing, in several cases giving up
entirely on
run-time enforcement of types @~local:cite{greenman2018spectrum}. TypeScript~\cite{local:ts}, Flow~\cite{local:flow},
MyPy~\cite{local:mypy}, and others omit dynamic checks, making their type
systems unsound. Others, such as Grace~\cite{local:grace} and
Reticulated Python~\cite{local:vitousek2014design}, keep some dynamic checking, but give up the full
soundness guarantee offered by gradual typing. Yet other systems, such
as Safe TypeScript~\cite{local:rastogi2015safe},
Nom~\cite{local:muehlboeck2017sound},
Thorn~\cite{local:wrigstad2017integrating},
and Dart~\cite{local:dart}
limit the interoperability possible in the
language to avoid some expensive checks.

@fig:overhead-summary
\begin{wrapfigure}{r}{0.55\textwidth}
  \includegraphics[width=0.55\textwidth]{figs/slowdown.ps}
  \caption{Overhead of gradual typing over the whole benchmark
    suite. The purple ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key0.ps}}}$) curve is Typed Racket and the orange ($\vcenter{\hbox{\protect\includegraphics[height=1em]{figs/key1.ps}}}$) curve is \tool.
    The log-scaled x-axis indicates slowdown factor compared against the
    fully-untyped configuration, while the y-axis indicates the
    percent of configurations incurring that slowdown. Each benchmark
    is allocated an equal proportion of the y-axis. Higher is better.}
\Description{CDF showing overall overhead is significantly better with
  SVC-CR compared to Typed Racket.}
  \label{fig:overall}
\end{wrapfigure}

\paragraph{Our key idea is that dynamic contracts are statically useful.}
We show that the dilemma of gradual type enforcement can be resolved
without giving up either the semantic benefits of soundness or
efficient execution.  Our tool, \tool, @emph{statically verifies} the
contracts generated by Typed Racket, an existing gradually-typed
language, eliminating those that cannot fail at run-time.
These contracts generate significant, useful
information which can be used to reason about the static behavior of
code, even in the absence of static types.  In particular, contracts
characterize the allowable interactions between typed and untyped
code, which can be used to validate that untyped code always respects
the type abstractions of its typed counterparts.  Building on a sound
and precise higher-order symbolic execution
system~\cite{local:Nguyen2018Soft}, \tool eliminates almost all of the
contracts generated by Typed Racket across a dozen pre-existing
benchmarks, reducing the performance overhead to almost none.  In
short, this work focuses on eliminating checks that are not going to
fail, rather than worrying about their expense.

A well-known theorem of typed functional programming, when put in
sloganized form, says ``well-typed programs don't go @emph{wrong}.''
It has been adapted to the setting of gradually-typed programming to
``well-typed modules can't be @emph{blamed}.''  Essentially, things
can go wrong at run-time, but it's always the untyped code's fault.
This is a lovely property, but one that perhaps paints untyped
code too broadly as unreasonable.  Research on gradually typed
languages usually treats untyped code as code for which all bets are
off. If we can't statically know anything about the untyped code, then
optimization must focus on the mechanisms of enforcing the disciplines of the
typed code within the wild frontier of the untyped code.

Our work begins instead from the hypothesis that ``untyped modules can be
blamed, @emph{but usually aren't}.''  In other words, untyped code may
not follow the static discipline of a given type system, but it often
does follow that discipline dynamically.  Moreover, the static
requirements, formulated as dynamic contracts, can be validated
against the untyped code.  What is needed is a verification method
able to more closely model dynamic idioms of untyped languages,
for which we find higher-order symbolic execution a good fit.


\paragraph{Contributions.}

This paper contributes:

\begin{itemize}
\item the idea that dynamic contracts are statically useful for
  optimizing gradually typed programs by verifying contracts against
  the untyped portions of a program,

\item a technique for reducing the problem of optimizing a gradually
  typed program into the problem of modular contract verification,

\item a tool that implements these ideas, integrating Typed Racket and
  an existing contract verification system based on higher-order
  symbolic execution,

\item and an evaluation demonstrating the remarkable effectiveness of this
  approach on a canonical gradual typing benchmark suite.

\end{itemize}

The overall performance of our system is visualized in the cumulative
distribution function (CDF) plot in
figure~\ref{fig:overall}. This plot follows the conventions of
\citet{local:takikawa2016}, and represents
the normalized percentage of configurations (on the y-axis)
that have less than the given slowdown (on the x-axis, log-scale).\footnote{
  The percent is normalized such that all benchmarks are weighed equally,
  even though some may contain many more configurations than others.
}
For example, Typed Racket 7.4 runs $46\%$ of benchmark
configurations with less than $2\times$ slowdown.
With \tool, $95\%$ of
benchmark configurations have less than $1.23\times$ slowdown
compared to the fully-untyped configuration.
As this plot makes clear, \tool reduces overhead to nearly zero in almost
all cases, and completely eliminates all overheads over $2\times$.

In the remainder of the paper, we describe our approach, why it works
well on gradual typing, and provide an extensive evaluation. We begin
with an example-driven overview of how contract verification can
eliminate gradual typing dynamic checks (\S\ref{sec:examples}). We
then describe the implementation (\S\ref{sec:impl}), including
integration with Typed Racket, use of an existing symbolic execution
engine, and subsequent optimization. We evaluate our tool
(\S\ref{sec:eval}) on a dozen pre-existing benchmarks drawn from
@emph{How to Evaluate the Performance of Gradual Typing Systems} by
\citet{local:greenman2019}, elaborating on the summary given in
figure~\ref{fig:overall}. Finally, we provide a perspective on how our
results change the landscape of gradual typing evaluation.
