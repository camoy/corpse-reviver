#lang scribble/acmart @acmsmall

@title{Related work}

Early on, developers of gradual type systems realized that the dynamic
checks involved could have significant performance impacts, spurring
the development of monotonic references~\cite{local:siek2015monotonic,
  local:swamy2014gradual} as a run-time enforcement mechanism, for
example.
%
Subsequently, Takikawa, Greenman, and their collaborators
\cite{local:takikawa2015towards,local:takikawa2016,local:greenman2019} made three major
contributions that focused attention on the problem: the design
of a method for analyzing and reporting gradual typing performance,
the creation of a suite of gradual typing benchmarks, and the
demonstration that Typed Racket as of 2015 had substantial performance
problems.


Since then, work in addressing gradual typing's performance challenges
has proceeded in three directions: developing new run-time mechanisms,
restricting the expressiveness of the system, and relaxing the
guarantees of sound interoperation.

\paragraph{Run-time improvements}
Many approaches to improving the run-time performance of gradual
typing attempt to simply execute existing dynamic checks more
efficiently. This can take the form of more efficient underlying
virtual machines, such as the Pycket tracing JIT~\cite{local:bauman2017mostly},
more efficient compilation of contracts~\cite{local:feltey2018collapsible}, or
entirely new compilers for gradually-typed languages~\cite{local:kuhlenschmidt2019toward}.

None of these systems is able to totally eliminate the overhead of
gradual typing; each suffers from at least $10\times$ slowdown in the
worst case. By taking a static verification perspective, instead of
dynamic optimization, \tool is able to totally remove expensive
contracts instead of optimizing them. For those contracts that remain,
improved run-time techniques may help accelerate them, but we leave
that investigation to future work.

\paragraph{Restricted languages}

In contrast to languages that optimize slow run-time checks, other
gradually-typed languages restrict interoperation to make it
impossible to create slow run-time checks. This includes systems such
as Nom~\cite{local:muehlboeck2017sound} and C\#~\cite{local:bierman2010adding}
that require all data to be created in the typed code and use nominal type
tags for dynamic checks. Other systems limit what can flow
across boundaries~\cite{local:swamy2014gradual,DBLP:journals/pacmpl/RichardsAT17,
local:wrigstad2017integrating, DBLP:conf/ecoop/RichardsNV15, local:dart}.

In contrast to these approaches, \tool imposes no limits on the Typed
Racket type system, on what kinds of untyped programs can be used
together with typed modules, or on what values can flow across
boundaries.

\paragraph{Relaxed soundness}
The most popular method for avoiding run-time overhead is of course to
entirely omit the dynamic checks needed for soundness.
This is the approach taken by almost all of the popular gradual type systems
developed outside of academia, including TypeScript, MyPy, Flow, Hack,
and others. The Sorbet system for Ruby includes some dynamic checks,
although the documentation is unclear on exactly what is checked.

\citet{local:vitousek2017big} show that by inserting first-order dynamic checks
throughout a program, a  limited notion of soundness can be
achieved, while avoiding the potentially-costly wrappers found in
other gradual type systems. Subsequently, \citet{local:greenman2018spectrum}
characterized this approach and others, while showing
that a preliminary implementation for Typed Racket was helpful in some
benchmarks. \citet{local:vitousek2019optimizing} demonstrate that with static
elimination of redundant checks, plus the addition of a JIT compiler,
almost all remaining overhead can be eliminated, although still
without the full guarantees of soundness or the precise error
reporting of other gradual type systems.

Our results demonstrate that with static contract verification,
there's no need to compromise on soundness or expressivity: the
performance results of section~\ref{sec:eval} are as good or better than any
other system with even limited soundness, while we retain the
semantics of Typed Racket.


\paragraph{Run-time check elimination}
Many systems have been designed to analyze untyped languages such as
Scheme~\cite{dvanhorn:Henglein1994Dynamic, dvanhorn:Wright1997Practical, dvanhorn:Cartwright1991Soft, dvanhorn:Flanagan1999Componential, dvanhorn:Aiken1994Soft}, or existing
languages with contract systems~\cite{dvanhorn:Xu2012Hybrid, dvanhorn:Xu2009Static, dvanhorn:Vytiniotis2013HALO, dvanhorn:Meunier2006Modular}, to avoid possible run-time errors,
similar to the {\sc scv} system~\cite{local:Nguyen2014Soft,local:Nguyen2015Relatively,local:Nguyen2018Soft} we build on. A
 discussion of the relations between these systems is provided
by~\citet{local:nguyen2017higher}.

%Henglein, Xu, Soft Scheme -- see
%SCV papers.
