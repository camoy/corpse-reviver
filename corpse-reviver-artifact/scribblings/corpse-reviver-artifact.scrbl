#lang scribble/acmart @acmsmall

@(require scriblib/figure
          "../private/figure.rkt"
          "../private/stat.rkt"
          "../private/util.rkt")

@title[#:style CORPSE-REVIVER-ARTIFACT-STYLE]{
  Artifact: Corpse Reviver
}
@author{Cameron Moy}
@author{Phúc C. Nguyễn}
@author{Sam Tobin-Hochstadt}
@author{David Van Horn}

@section{Installing}

@subsection{VirtualBox (Recommended)}

Install @hyperlink["https://docker.com/"]{Docker}.

@verbatim{
$ docker pull camoy/corpse-reviver
$ docker run -it camoy/corpse-reviver
}

@subsection{Local}

Install @hyperlink["https://download.racket-lang.org"]{Racket 7.8}.

@verbatim{
$ raco pkg install --clone https://github.com/camoy/corpse-reviver
}

@section{Custom Program}

@section{Benchmarking}

@section{Claims}

@subsection{Figures}

@figure["overhead-summary"]{
@elem{
Overhead of gradual typing over the whole benchmark suite.
The purple (@|purple-key|) curve is Typed Racket
and the orange (@|orange-key|) curve is @|scv-cr|.
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
Typed Racket 7.7 on the left
and @scv-cr on the right.
Red indicates a slowdown over @format-overhead[3]
and green indicates a slowdown below @format-overhead[1.25].
Note that all @scv-cr entries are green.
}
@lattices
}

@figure["fig:overhead-grid"]{
@elem{
Overhead of gradual typing for each benchmark individually.
The purple curve is Typed Racket and the orange curve is @|scv-cr|.
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
The purple curve is Typed Racket and the orange curve is @|scv-cr|.
The x-axis is binned by the number of typed modules in a configuration.
The y-axis is time to execute in seconds.
}
@exact-grid
}

@figure["table:summary"]{
@elem{
Maximum and mean overhead for Racket 7.7 and @scv-cr for each benchmark.
Red indicates a slowdown ≥@format-overhead[3]
while green indicates a slowdown ≤@format-overhead[1.25].
}
@table-summary
}

@subsection{Empirical}

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
For example, Typed Racket 7.7 runs @%-2x-baseline of benchmark configurations
with less than @format-overhead[2] slowdown.
With @|scv-cr|,
@format-percent[0.95] of benchmark configurations have less than
@95%-quantile-opt slowdown compared to the fully-untyped configuration.
As this plot makes clear,
@scv-cr reduces overhead to nearly zero in almost all cases,
and completely eliminates all overheads over @|max-opt|.
}

@item{
The worst overhead incurred
by gradual typing with @scv-cr
is a slowdown of @|max-opt|.
Only @%-baseline-within-max-opt of benchmark configurations without
contract verification are within this slowdown factor,
while the largest overhead exceeds @max-baseline overhead.
}

@item{
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
