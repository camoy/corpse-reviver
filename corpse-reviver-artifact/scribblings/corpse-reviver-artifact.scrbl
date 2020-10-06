#lang scribble/manual

@require[(for-label typed/racket/base)
         scriblib/figure
         "../private/figure.rkt"
         "../private/stat.rkt"
         "../private/util.rkt"]

@title[#:style CORPSE-REVIVER-ARTIFACT-STYLE]{
  Artifact: Corpse Reviver
}
@author{Cameron Moy}
@author{Phúc C. Nguyễn}
@author{Sam Tobin-Hochstadt}
@author{David Van Horn}

This artifact accompanies the paper
@hyperlink["https://arxiv.org/abs/2007.12630"]{
``Corpse Reviver: Sound and Efficient Gradual Typing via Contract Verification.''
}
Artifact evaluation phase I consists of
the @secref{install} and @secref{opt} sections.
Artifact evaluation phase II consists of
the @secref{benchmark} and @secref{results} sections.

@section[#:tag "install"]{Install}

You can run the artifact in a VirtualBox virtual machine
that includes all the necessary dependencies.
We recommend this method of installation.
Alternatively,
you can manually install the artifact
on any Unix-like operating system.

@subsection{VirtualBox}

@itemlist[
@item{
Download @hyperlink["https://www.virtualbox.org/wiki/Downloads"]{VirtualBox.}
}
@item{
Download the artifact's @hyperlink["http://camoy.name"]{virtual machine image}.
}
@item{
Open VirtualBox,
choose File → Import Appliance,
select the downloaded @exec{ova} file,
and start the virtual machine.
}]

@subsection{Manual}

@itemlist[
@item{
Install @hyperlink["https://download.racket-lang.org/all-versions.html"]{Racket 7.8}.
}
@item{
Run the following commands to download and install SCV-CR:
@verbatim{
$ git clone https://github.com/camoy/corpse-reviver
$ raco pkg install corpse-reviver/corpse-reviver-artifact \
                   corpse-reviver/corpse-reviver-benchmark \
                   corpse-reviver/corpse-reviver
}
}]

@section[#:tag "opt"]{Optimizing a program}

You can write your own gradually typed program
and see how well SCV-CR can optimize it.
Place the following code in a file called
@exec{data.rkt}:

@figure["fig:typed-data"]{
@elem{The @exec{data.rkt} file.}
@codeblock0{
#lang typed/racket/base

(provide random-adders)

(: random-adders : (-> Natural (Listof (-> Integer Integer))))
(define (random-adders n)
  (for/list ([i (in-range n)])
    (λ ([x : Integer]) (+ (random 10) x))))
}
}

This typed module provides a function called
@code{random-adders}
that generates a list of random ``adders,''
functions that add its argument to a random number.
Now, place the following code in a file called
@exec{main.rkt}:

@figure["fig:untyped-main"]{
@elem{The @exec{main.rkt} file.}
@codeblock0{
#lang racket/base

(require "data.rkt")

(define iterations 1000)
(define n 5000)

(time
 (for ([i (in-range iterations)])
   (for/sum ([f (in-list (random-adders n))])
     (f (random 10)))))
}
}

This untyped module generates
a list of 5,000 adders from @exec{data.rkt}
and computes a sum with them.
It does this for 1,000 iterations
and measures how long it takes.

Run the program and use Racket's profiler
to see the cost of contracts due to gradual typing.
On our machine,
the program takes about 10 seconds
with about 67% of that time due to contract checking.

@verbatim{
$ raco make data.rkt main.rkt
$ racket main.rkt
cpu time: 9398 real time: 9412 gc time: 874
$ raco contract-profile main.rkt
Running time is 66.66% contracts
8014/12022 ms

(-> natural? (listof (-> exact-integer? any)))     8013.5 ms
server.rkt:3:9
    random-adders                                  8013.5 ms
$ raco decompile main.rkt > unopt.rkt
}

Run SCV-CR on the modules first
and see the performance difference.
In this simple example,
the tool proves that the contracts
cannot be violated
and removes them all automatically.

@verbatim{
$ raco scv-cr data.rkt main.rkt
$ racket main.rkt
cpu time: 1256 real time: 1258 gc time: 28
$ raco contract-profile main.rkt
cpu time: 1406 real time: 1410 gc time: 8
Running time is 0% contracts
0/2340 ms
$ raco decompile main.rkt > opt.rkt
}

For further confirmation that the contracts were optimized,
you can compare the decompiled outputs in
@exec{unopt.rkt}
and @exec{opt.rkt}.

@verbatim{
$ diff opt.rkt unopt.rkt | grep "random-adders '5000" -A 2
<          (let ((local67 (random-adders '5000)))
---
>          (let ((local73 (lifted/12.1 '5000)))
}

On top is the optimized version that directly calls @exec{random-adders}.
The bottom,
by contrast,
calls @exec{lifted/12.1} instead.
If you chase down this definition,
it is the contracted version of @exec{random-adders}.

See
@seclink["top"
        #:tag-prefixes
        '("(lib corpse-reviver/scribblings/corpse-reviver.scrbl)")]{
  the SCV-CR documentation
}
for the list of options to @exec{raco scv-cr}.

@section[#:tag "benchmark"]{Benchmark}

Execute the following commands to
run the entire benchmark suite:

@verbatim{
$ cd corpse-reviver/corpse-reviver-artifact
$ raco scv-cr-benchmark -c 5 -b -i 5 -S 2 -R 2 -o data/baseline
$ raco scv-cr-benchmark -r -o data/opt
}

These commands will create a series of
JSON-formatted measurement files---one
for each benchmark sample.
Baseline measurements,
without applying SCV-CR,
are placed in @exec{data/baseline}.
Measurements after applying SCV-CR
are placed in @exec{data/opt}.

We have found that on a decent laptop,
the benchmarks with these parameters
terminate in about 14 hours.
If you want to run this script overnight,
combine the last two commands with
@exec{&&}.

The suggested flags will sample from
larger benchmarks instead of
exhaustively measuring them all.
Different parameter choices will yield
different benchmarking times.
Changing @exec{-R} from 2 to 1
will halve benchmark completion time.
We don't recommend going any lower than this setting.
If you want to run the benchmarks for longer,
increasing any one of
@exec{-S},
@exec{-R},
or @exec{-c},
will result in a more accurate result.
See
@seclink["top"
        #:tag-prefixes
        '("(lib corpse-reviver-benchmark/scribblings/corpse-reviver-benchmark.scrbl)")]{
  the benchmarking documentation
}
for more information.

After this is done,
you can generate this page
with your benchmark results.

@verbatim{
$ raco setup corpse-reviver-artifact
$ raco docs T:corpse-reviver-artifact
}

When @exec{raco docs} opens a browser,
select the first result.
The page you're currently reading should appear,
but the figures and claims in @secref{results}
will be generated from your data instead
of ours (available in the
@exec{author_data}
directory).

@section[#:tag "results"]{Empirical results}

Compare the figures and claims
generated from your benchmarking results
to those generated from ours.
Depending on your choice of parameters,
the results may differ more or less.
However,
they should generally support
the thesis of the paper---that
across a wide range of benchmarks,
contract verification can effectively
reduce the performance overhead
of sound gradual typing.

@subsection{Figures}

@figure["overhead-summary"]{
@elem{
Overhead of gradual typing over the whole benchmark suite.
The purple (@|purple-key|) curve is Typed Racket
and the orange (@|orange-key|) curve is SCV-CR.
The log-scaled x-axis indicates slowdown factor
compared against the
fully-untyped configuration,
while the y-axis indicates the
percent of configurations incurring that slowdown.
Each benchmark is allocated
an equal proportion of the y-axis.
Higher is better.
}
@overhead-summary
}

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
Typed Racket @baseline-version on the left
and SCV-CR on the right.
}
@lattices
}

@figure["fig:overhead-grid"]{
@elem{
Overhead of gradual typing for each benchmark individually.
The purple curve is Typed Racket and the orange curve is SCV-CR.
Each point (x, y) indicates an x-factor slowdown
over the fully-untyped configuration for y% of configurations.
The dashed lines between 1 and 2 occur at increments of 0.2
and between 2 and 10 at increments of 2.
}
@overhead-grid
}

@figure["fig:exact-grid"]{
@elem{
Exact time it takes each configuration to execute.
The purple curve is Typed Racket and the orange curve is SCV-CR.
The x-axis is binned by the number of typed modules in a configuration.
The y-axis is time to execute in seconds.
}
@exact-grid
}

@figure["table:summary"]{
@elem{
Maximum and mean overhead for
Racket @baseline-version
and SCV-CR
for each benchmark.
Red indicates a slowdown ≥@format-overhead[3]
while green indicates a slowdown ≤@format-overhead[1.25].
}
@table-summary
}

@subsection{Supporting claims}

@itemlist[
@item{
We evaluate this approach on a dozen existing gradually-typed
programs previously shown to have prohibitive performance overhead---with
a median overhead of @median-baseline and up to
@max-baseline in the worst case---and
eliminate all overhead in most cases and suffer
@max-opt in the worst case.
}

@item{
For example,
Typed Racket @baseline-version runs @%-2x-baseline of benchmark configurations
with less than @format-overhead[2] slowdown.
With SCV-CR,
@format-percent[0.95] of benchmark configurations have less than
@95%-quantile-opt slowdown compared to the fully-untyped configuration.
As this plot makes clear,
SCV-CR reduces overhead to nearly zero in almost all cases,
and completely eliminates all overheads over @|max-opt|.
}

@item{
The worst overhead incurred
by gradual typing with SCV-CR
is a slowdown of @|max-opt|.
Only @%-baseline-within-max-opt of benchmark configurations without
contract verification are within this slowdown factor,
while the largest overhead exceeds @max-baseline overhead.
}

@item{
Take the @sc{sieve} benchmark as an example.
The baseline performance begins at 50%,
meaning two of the four
configurations are within a @format-overhead[1] slowdown
of the fully-untyped configuration.
From @figure-ref{fig:lattices}, these are the fully-untyped
configuration itself and the fully-typed configuration.
The one-time increase in the CDF shows the configuration
that has @sieve-small overhead.
We never see the CDF reach @format-percent[1] since
this would occur at @|sieve-large|,
beyond the x-axis's range.
}

@item{
There are no pathological cases
and any path through the lattice of configurations
from untyped to typed programs exhibit
at most @|max-opt| slowdown.
}
]
