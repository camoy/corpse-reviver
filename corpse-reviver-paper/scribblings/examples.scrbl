#lang corpse-reviver-paper

@require[(for-label typed/racket/base)]

@title{Examples and intuition}

This section explains
how sound type enforcement
significantly slows down a simple gradually-typed program,
and describes how contract verification
helps eliminate this overhead.

@section{A small benchmark}

The @sc{sieve} program is a synthetic benchmark
constructed as a small example that
exhibits major performance problems
in a gradually-typed setting.
It computes prime numbers using the
Sieve of Eratosthenes algorithm
over a lazy stream data structure.
Only one boundary is present in the program,
between the @tt{streams} library
and the @tt{main} driver module.

@; TODO: Join these figs? See ADI online version.

@figure["fig:typed-streams"]{
@elem{The fully-typed @tt{streams} module from the @sc{sieve} benchmark.}
@codeblock0{
#lang typed/racket
(provide (struct-out stream)
         stream-unfold
         stream-get)

(struct: stream ([first : Natural] [rest : (-> stream)]))

(: stream-unfold (-> stream (values Natural stream)))
(define (stream-unfold st)
  (values (stream-first st) ((stream-rest st))))

(: stream-get (-> stream Natural Natural))
(define (stream-get st i)
  (define-values (hd tl) (stream-unfold st))
  (if (= i 0)
      hd
      (stream-get tl (sub1 i))))
}
}

@figure["fig:typed-main"]{
@elem{The fully-typed @tt{main} module from the @sc{sieve} benchmark.}
@codeblock0{
#lang typed/racket
(require "streams.rkt")

(: count-from (-> Natural stream))
(define (count-from n)
  (stream n (λ () (count-from (add1 n)))))

(: sift (-> Natural stream stream))
(define (sift n st)
  (define-values (hd tl) (stream-unfold st))
  (if (= 0 (modulo hd n))
      (sift n tl)
      (stream hd (λ () (sift n tl)))))

(: sieve (-> stream stream))
(define (sieve st)
  (define-values (hd tl) (stream-unfold st))
  (stream hd (λ () (sieve (sift hd tl)))))

(: primes stream)
(define primes (sieve (count-from 2)))
(stream-get primes 6666)
}
}

@Figure-ref{fig:typed-streams} shows the @tt{streams} module
that implements an infinite stream as a structure containing
the next element in the stream,
and a thunk that computes the rest of the stream when applied.
Stream operations include
@tt{stream-unfold} that returns a stream's next element and forces its rest,
and @tt{stream-get} that returns the stream's i@th element.

@Figure-ref{fig:typed-main} shows the @tt{main} module
that computes the prime numbers as an infinite stream,
and queries the 6666@th prime.
Included are three ancillary functions:
@tt{count-from} returns an infinite stream of natural numbers
starting from a lower bound,
@tt{sift} filters out elements
divisible by a given number,
and @tt{sieve} filters out elements
that are divisible by a preceding element.
All prime numbers can be computed
by filtering the naturals
starting at 2
with @tt{sieve}.

A gradually-typed language
permits us to incrementally
add types to a program
while still allowing
mixed-typed configurations to run.
In Typed Racket,
the units of migration are whole modules,
so for @sc{sieve} there are 4 runnable configurations.
@Figure-ref{fig:typed-streams} and @figure-ref{fig:typed-main}
make up the fully-typed configuration
after migrating both untyped modules.

We chose this example because it is relatively small,
and the interaction between @tt{main} and @tt{streams}
involve wrapped functions that incur substantial slowdown
from dynamic checks.

@section{Source of the slowdown}

Consider a point along the migration path between
the fully-untyped and fully-typed configurations.
Suppose @tt{streams} is typed and @tt{main} is untyped.
To ensure @tt{streams} is protected when interacting with @tt{main},
Typed Racket generates contracts that enforce the type invariants
on values that flow from untyped to typed modules,
as shown in @figure-ref{fig:untyped-sieve}.
In our example,
each time the untyped @tt{main} module invokes the @tt{stream} constructor,
the first element is checked against a flat contract to ensure that
it is a natural number.
This obligation is discharged immediately,
yielding either a contract violation
or passing the value forward.
The rest of the stream, a thunk, is wrapped in
a proxy@~cite[findler-2002]
to guarantee that it returns a @tt{stream} when called.

@; TODO: These links are wrong (since we imported typed/racket/base
@; for-label.

@figure["fig:untyped-sieve"]{
@elem{
The fully-typed @tt{streams} module
as an untyped module
with explicit contracts
for the @sc{sieve} benchmark.
The elided code
is the same as the corresponding code
in @figure-ref{fig:typed-streams},
minus the type annotations.
}
@codeblock0{
#lang racket
(provide
 (contract-out
  [stream-get    (-> stream? natural? natural?)]
  [stream-unfold (-> stream? (values natural? stream?))]
  [struct stream ([first natural?] [rest (-> stream?)])]))
...
}
}

Unfortunately,
in this configuration,
an enormous number of values flow through the boundary
between the untyped and typed modules.
Computing the 6666@th prime number
results in just under 45 million thunk allocations and applications.
In general,
computing the n@th prime
requires at least a quadratic number of calls to @tt{sift},
implying a significant amount of checking and wrapping.

@section{Contract verification}

Eliminating run-time checks requires verifying
the untyped @tt{main}
using the contracts generated by
the typed @tt{streams}.
Specifically, the contracts from @tt{streams}
that enforce its client's behavior are:

@itemlist[
@item{
@tt{natural?} and @tt{(-> stream?)}
for the stream constructor's arguments,
}
@item{@tt{stream?} for @tt{stream-unfold}'s argument,}
@item{and @tt{stream?} and @tt{natural?} for @tt{stream-get}'s arguments.}
]

If we can prove that @tt{main} never violates these contracts,
then they are redundant and can be eliminated without
changing the program's behavior.

Verification of @tt{main} involves approximating arbitrary
interactions with it through symbolic execution.
If @tt{main} is blame-free during symbolic execution,
it must also be blame-free in any concrete execution,
by the soundness of higher-order symbolic execution@~cite[nguyen-2018].

Because @tt{main} does not export any values, the only possible
interaction with @tt{main} is running it, and the only
non-trivial expressions to evaluate are the last two,
constructing the infinite prime stream,
and querying the 6666@th element.
We consider how each function application in these two expressions
are symbolically evaluated.

@nested[#:style 'inset]{
@tt{count-from}.
To define @tt{primes}, @tt{main} calls @tt{count-from} with 2,
in turn calling the stream constructor with 2
and a thunk that recursively calls @tt{count-from}.
The former satisfies the flat contract @tt{natural?},
and the latter is wrapped in higher-order contract @tt{(-> stream?)}.
Consequently, @tt{count-from} returns a @tt{stream} containing a natural number,
and a guarded thunk whose return value
will be monitored to satisfy @tt{stream?}.

@tt{sieve}.
When @tt{sieve} is applied to this result,
the stream is passed to @tt{stream-unfold},
whose argument contract @tt{stream?} is satisfied.
From @tt{main}'s point of view, the @tt{stream-unfold} function is opaque;
its behavior is described only by its contract.
Therefore,
symbolic execution simulates the arbitrary ways @tt{stream-unfold}
could interact with its context---how it can return
and use its higher-order argument.
Here, the approximation of @tt{stream-unfold}
repeatedly explores applications of the stream's rest,
its rest's rest, and so on.
Each time a new stream flows to the unknown,
its guarded thunk correctly applies the @tt{stream} constructor
to a natural number and a thunk, and returns a stream
that satisfies @tt{stream?}.
When @tt{stream-unfold} returns, its contract guarantees
that @tt{hd} is a natural number, and @tt{tl} is a stream.
The @tt{sieve} function then applies the stream constructor
on the symbolic value @tt{hd} satisfying contract @tt{natural?}
and thunk wrapped in the higher-order contract @tt{(-> stream?)}.


@tt{stream-get}.
In the call to @tt{stream-get}, @tt{main} satisfies
both of the argument contracts @tt{stream?} and @tt{natural?}.
Since @tt{stream-get} is opaque from @tt{main}'s point of view,
the stream @tt{primes} is explored arbitrarily as before.
When the guarded thunk in @tt{streams} is forced, it triggers recursive
calls to @tt{sieve} and @tt{sift}, whose symbolic execution proceeds similarly.
At all points in symbolic execution,
applications to the stream constructor correctly have the first
argument be a natural number, and the second argument be a thunk
that produces a stream when forced.
Moreover, all applications of @tt{stream-unfold} and @tt{stream-get}
also respect the functions' contracts.
}

Although symbolic execution may need to explore an infinite state space,
we allow termination by applying well-studied techniques for
systematically finitizing an existing semantics to obtain a sound
over-approximation@~cite[darais-2017 van-horn-2010].
Soundness means that an over-approximated symbolic execution that terminates
with no blame on @tt{main} implies that @tt{main} is blame-free in the concrete.

@section{Optimization and evaluation}

@figure["fig:lattices"]{
@elem{
Performance lattices for
@sc{sieve} (left)
and @sc{zombie} (right).
Each point in the lattice
is a configuration of the benchmark,
where a white box is an untyped module
and a black box is a typed module.
The numbers below indicate the slowdown factor for
Typed Racket 7.7 on the left
and @scv-cr on the right.
Red indicates a slowdown over @format-overhead[3]
and green indicates a slowdown below @format-overhead[1.25].
Note that all @scv-cr entries are green.
}
@fig:lattices
}

In the case of @sc{sieve},
all contracts are fully verified
in every configuration.
Soundness of the verifier
permits us to safely
bypass all contracts
generated by Typed Racket,
since they cannot
be violated at run time.
A configuration may,
in general,
fail to verify completely.
This requires identifying
contracts that do not verify
and keeping them in the compiled program.

The left side of @figure-ref{fig:lattices}
is a performance lattice
that visualizes the performance improvement for all configurations
of @sc{sieve}.
Each point in the lattice represents a configuration of the program,
consisting of a box for each module.
A white box is an untyped module and a black box is a typed module.
Performance lattices are ordered by the subset relation
on the set of typed modules,
so the fully-untyped configuration is the bottom element of the lattice,
while the fully-typed configuration is the top element.
Below each configuration are two numbers
the left corresponds to the unoptimized overhead
of the configuration compared to the fully-untyped version,
and the right corresponds to the configuration's overhead after
optimization with @|scv-cr|.
Since all configurations fully verify,
gradual typing imposes no overhead at all.
Hence,
the performance of every @scv-cr optimized program is @format-overhead[1],
exactly the same as the fully-untyped configuration.
