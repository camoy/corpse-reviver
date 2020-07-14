#lang scribble/acmart @acmsmall

@(require scriblib/figure
          "../private/figure.rkt")

@title{Examples and intuition}

This section explains how sound type enforcement significantly slows down
a simple gradually-typed program,
and describes how contract verification helps eliminate this overhead.

@section{A small benchmark}

The {\sc sieve} program is a synthetic benchmark
constructed as a small example that
exhibits major performance problems
in a gradually-typed setting.
It computes prime numbers using the
Sieve of Eratosthenes algorithm
over a lazy stream data structure.
The program consists of two modules:
a {\tt streams} library
and a {\tt main} driver module.


\begin{figure*}[h]
\begin{subfigure}[t]{0.5\textwidth}
\begin{Verbatim}[fontsize=\small]
#lang typed/racket
(provide (struct-out stream)
         stream-unfold
         stream-get)

(struct: stream
  ([first : Natural]
   [rest : (-> stream)]))

(: stream-unfold
   (-> stream (values Natural stream)))
(define (stream-unfold st)
  (values (stream-first st)
          ((stream-rest st))))

(: stream-get (-> stream Natural Natural))
(define (stream-get st i)
  (define-values (hd tl)
    (stream-unfold st))
  (if (= i 0)
      hd
      (stream-get tl (sub1 i))))
\end{Verbatim}
\caption{The fully-typed \texttt{streams} module.}
\label{fig:typed-sieve-streams-code}
\end{subfigure}
~
\begin{subfigure}[t]{0.5\textwidth}
\begin{Verbatim}[fontsize=\small,commandchars=\\\{\}]
#lang typed/racket
(require "streams.rkt")

(: count-from (-> Natural stream))
(define (count-from n)
  (stream n (\ensuremath{λ} () (count-from (add1 n)))))

(: sift (-> Natural stream stream))
(define (sift n st)
  (define-values (hd tl) (stream-unfold st))
  (if (= 0 (modulo hd n))
      (sift n tl)
      (stream hd (\ensuremath{λ} () (sift n tl)))))

(: sieve (-> stream stream))
(define (sieve st)
  (define-values (hd tl) (stream-unfold st))
  (stream hd (\ensuremath{λ} () (sieve (sift hd tl)))))

(: primes stream)
(define primes (sieve (count-from 2)))
(stream-get primes 6666)
\end{Verbatim}
\caption{The fully-typed \texttt{main} module.}
\label{fig:typed-sieve-main-code}
\end{subfigure}
\caption{The fully-typed configuration of {\sc sieve}.}
\label{fig:typed-sieve-code}
\end{figure*}

The {\tt streams} module in figure~\ref{fig:typed-sieve-streams-code}
implements an infinite stream as a structure containing
the next element in the stream,
and a thunk that computes the rest of the stream when applied.
Stream operations include
{\tt stream-unfold} that returns a stream's next element and forces its rest,
and {\tt stream-get} that returns the stream's $i^{\textit{th}}$ element.

The {\tt main} module in figure~\ref{fig:typed-sieve-main-code}
computes the prime numbers as an infinite stream
and queries the \num{6666}$^{\textit{th}}$ prime.
The algorithm needs three ancillary functions:
{\tt count-from} returns an infinite stream of natural numbers starting from a lower bound,
{\tt sift} filters out elements divisible by a given number,
and {\tt sieve} filters out elements that are divisible by a preceding element.
The prime numbers are computed from the naturals starting at {\tt 2},
filtered by {\tt sieve}.

A gradually-typed language
permits us to incrementally
add types to a program
while still allowing
a mixed-typed configuration to run.
In Typed Racket,
the units of migration are whole modules,
so for {\sc sieve} there are $4$ runnable configurations.
Figure~\ref{fig:typed-sieve-code}
shows the fully-typed configuration---the finished product
after migrating both untyped modules.

We chose this example because it is relatively small,
and the interaction between {\tt main} and {\tt streams}
involve wrapped functions that incur substantial slowdown
from dynamic checks.

@section{Source of the slowdown}

Consider a point along the migration path between
the fully-untyped and fully-typed configurations.
Suppose {\tt streams} is typed and {\tt main} is untyped.
To ensure {\tt streams} is protected when interacting with {\tt main},
Typed Racket generates contracts that enforce the type invariants
on values that flow from untyped to typed modules.
In our example,
each time an untyped module invokes the {\tt stream} constructor,
the first element is checked against a flat contract ensuring that
it is a natural number.
This obligation is discharged immediately,
yielding either a contract violation
or passing the value forward.
The rest of the stream, a thunk, is wrapped in
a proxy~\cite{dvanhorn:Findler2002Contracts}
to guarantee that it returns a {\tt stream} when called.

Unfortunately,
in this configuration,
tons of values flow through the boundary
between the untyped and typed modules.
The program computing the \num{6666}$^{\textit{th}}$ prime number
results in just under 45 million thunk allocations and applications.
In general,
computing the $i^{\textit{th}}$ prime
requires at least a quadratic number of calls to {\tt sift},
implying a significant amount of checking and wrapping.

@section{Contract verification}

Eliminating run-time checks requires verifying
the source code of the untyped {\tt main}
against the contracts generated by
the typed {\tt streams}.
Specifically, the contracts from {\tt streams}
that enforce its client's behavior are:
\begin{itemize}
\item {\tt natural?} and {\tt (-> stream?)}
  for the stream constructor's arguments,
  % We're omitting accessors for simplicity since they're not used
\item {\tt stream?} for {\tt stream-unfold}'s argument, and
\item {\tt stream?} and {\tt natural?} for {\tt stream-get}'s arguments.
\end{itemize}
If we can prove {\tt main} never violates these contracts,
then they are redundant and can be eliminated without
changing the program's behavior.

Verification of {\tt main} involves approximating arbitrary
interactions with it through symbolic execution.
If {\tt main} is blame-free in symbolic execution,
it must also be blame-free in any concrete execution
that places {\tt main} in a context,
by the soundness of higher-order symbolic execution~\cite{local:Nguyen2018Soft}.

Because {\tt main} does not export any values, the only possible
interaction with {\tt main} is running it, and the only
non-trivial expressions to evaluate are the last two,
constructing the infinite prime stream,
and querying the \num{6666}$^{\textit{th}}$ element.

% count-from
The {\tt main} module calls {\tt count-from} with {\tt 2},
in turn calling the stream constructor with {\tt 2}
and a thunk that recursively calls {\tt count-from}.
The former satisfies the flat contract {\tt natural?},
and the latter is wrapped in higher-order contract {\tt (-> stream?)}.
Consequently, {\tt count-from} returns a {\tt stream} containing a natural number,
and a guarded thunk whose return value
will be monitored to satisfy {\tt stream?}.

@figure["fig:lattices"]{
@elem{
Performance lattices for \textsc{sieve} and \textsc{zombie}.
Each point in the lattice is a configuration of the benchmark, where a white box is an untyped module and a black box is a typed module.
The numbers below indicate the slowdown factor for Typed Racket $7.4$ on the left and \tool on the right.
Red indicates a slowdown $\geq 3\times$ and green indicates a slowdown $\leq 1.25\times$.
}
@fig:lattices
}

\begin{figure}
\begin{minipage}{0.45\textwidth}
\begin{Verbatim}[fontsize=\small]
#lang racket
(provide
(contract-out
 [stream-get
  (-> stream? natural? natural?)]
 [stream-unfold
  (-> stream? (values natural? stream?))]
 [struct stream
  ([first natural?] [rest (-> stream?)])]
 )) ...
\end{Verbatim}
% \caption{The \texttt{streams} module as an untyped module with explicit contracts.}
\caption{The fully-typed {\tt streams} module as an untyped module with explicit contracts for the {\sc sieve} benchmark. The elided code is the same as the corresponding code in figure~\ref{fig:typed-sieve-streams-code}, minus the type annotations.}
\label{fig:untyped-sieve-code}
\end{minipage}
\hspace{1em}
\begin{minipage}{0.49\textwidth}
  \begin{subfigure}{0.20\textwidth}
    \includegraphics[width=\textwidth]{figs/sieve-lattice.ps}
  \end{subfigure}
  \hspace{0.5em}
  ~
  \begin{subfigure}{0.80\textwidth}
  \includegraphics[width=\textwidth]{figs/zombie-lattice.ps}
  \end{subfigure}
  \caption{}
  \label{fig:lattices}
\end{minipage}
\end{figure}
% sieve
When {\tt sieve} is applied to this stream,
the stream is passed to \texttt{stream-unfold},
whose argument contract {\tt stream?} is satisfied.
From {\tt main}'s point of view, the {\tt stream-unfold} function is opaque;
its behavior is described only by its contract.
Therefore,
symbolic execution simulates the arbitrary ways {\tt stream-unfold}
can interact with its context---how it can return
and use its higher-order argument.
Here, the approximation of {\tt stream-unfold}
repeatedly explores the applications of the stream's rest,
its rest's rest, and so on.
Each time a new stream flows to the unknown,
its guarded thunk correctly applies the {\tt stream} constructor
to a natural number and a thunk, and returns a stream
that satisfies {\tt stream?}.
%
When {\tt stream-unfold} returns, its contract guarantees
that {\tt hd} is a natural number, and {\tt tl} is a stream.
The {\tt sieve} function then applies the stream constructor
on the symbolic value {\tt hd} satisfying contract {\tt natural?}
and thunk wrapped in the higher-order contract {\tt (-> stream?)}.

% stream-get
In the call to {\tt stream-get}, {\tt main} satisfies
both of the argument contracts {\tt stream?} and {\tt natural?}.
Since {\tt stream-get} is opaque from {\tt main}'s point of view,
the stream {\tt primes} is explored arbitrarily as before.
When the guarded thunk in {\tt streams} is forced, it triggers the recursive
calls to {\tt sieve} and {\tt sift}, whose symbolic execution proceeds
similar to previous applications.
At all points in symbolic execution,
applications to the stream constructor correctly have the first
argument being natural numbers, and second argument being a thunk
that produces a stream when forced.
Moreover, all applications to {\tt stream-unfold} and {\tt stream-get}
also respect the functions' contracts.

% finitized
Although symbolic execution may need to explore an infinite state space,
we allow termination by applying well-studied techniques for
systematically finitizing an existing semantics to obtain its sound
over-approximation~\cite{local:darais2017adi,dvanhorn:VanHorn2010Abstracting}.
Thanks to soundness, an over-approximated symbolic execution that terminates
with no blame on {\tt main} implies that {\tt main} is blame-free in the concrete.


@section{Optimization and evaluation}

In the case of \textsc{sieve},
all contracts are fully verified
in every configuration.
Soundness of the verifier
permits us to safely
bypass all of the contracts
generated by Typed Racket,
since they cannot
be violated at run-time.
In general,
a configuration may fail to verify
completely.
This requires identifying
contracts that do not verify
and keeping them in the compiled program.

The left side of figure~\ref{fig:lattices} is a performance lattice
that visualizes the performance improvement for all configurations
of \textsc{sieve}.
Each point in the lattice represents a configuration of the program,
consisting of a box for each module.
A white box is an untyped module and a black box is a typed module.
Performance lattices are ordered by the subset relation
on the set of typed modules,
so the fully-untyped configuration is the bottom element of the lattice,
while the fully-typed configuration is the top element.
Below each configuration are two numbers,
the left corresponds to the unoptimized overhead
of the configuration compared to the fully-untyped configuration,
and the right corresponds to the configuration after
optimization with \tool.
Since all configurations fully verify,
gradual typing imposes no overhead at all.
Hence,
the performance of every \tool optimized program is
$1\times$,
exactly the same as the fully-untyped configuration.
