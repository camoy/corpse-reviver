#lang scribble/manual

@require[@for-label[corpse-reviver-benchmark
                    racket/base]]

@title{Benchmark: Corpse Reviver}
@author{Cameron Moy}

This module provides a command-line benchmarking script
built for the purpose of
measuring the performance benefit of SCV-CR.
Two types of JSON output are produced:
analysis results
and run-time results.
Analysis results collect
information during SCV-CR optimization
such as blame data.
Run-time results measure
benchmark performance
after optimization.

@section{Command Line}

The @exec{raco scv-cr-benchmark} command
accepts a list of
@tech[#:doc '(lib "gtp-measure/scribblings/gtp-measure.scrbl")
      #:key "gtp-typed/untyped-target"]{GTP typed/untyped targets}
to benchmark.
If none are provided
it will use a 12-program subset of
@other-doc['(lib "gtp-benchmarks/scribblings/gtp-benchmarks.scrbl")].
It accepts the following
command-line flags:

@itemlist[
@item{
  @exec{-b} or @exec{--baseline} ---
  Run baseline measurements,
  in other words,
  don't optimize with SCV-CR.
}

@item{
  @exec{-c} or @exec{--cutoff} ---
  Set the maximum number of components
  to measure exhaustively
  before sampling.
}

@item{
  @exec{-g} or @exec{--gc-log} ---
  Log garbage collection statistics
  during analysis.
  This can cause the analysis
  to slow down significantly;
  that's why it's disabled by default.
}

@item{
  @exec{-i} or @exec{--iterations} ---
  Set the number of iterations
  to run for each configuration.
}

@item{
  @exec{-n} or @exec{--no-skip} ---
  Don't skip the analysis of modules
  prefixed with @exec{_}.
  This the convention for indicating
  modules that should be treated as opaque.
}

@item{
  @exec{-o} or @exec{--output} ---
  Set the directory
  where analysis results
  will be placed.
  By default,
  it is the current working directory.
}

@item{
  @exec{-r} or @exec{--resume} ---
  Resume an existing setup.
  In other words,
  run the experiment described by
  existing @litchar{in} files in
  @litchar{$XDG_DATA_HOME/gtp-measure}.
  After running a baseline experiment
  you should use this option
  for the optimized experiment
  so it uses the same samples as the baseline.
}

@item{
  @exec{-w} or @exec{--worker-count} ---
  Set the number of parallel workers
  to use for measuring.
  By default,
  this is @racket[1].
  Each worker is given one benchmark to process
  at a time.
}

@item{
  @exec{-R} or @exec{--num-samples} ---
  Set the number of samples to take
  when measuring a benchmark approximately.
}

@item{
  @exec{-S} or @exec{--sample-factor} ---
  Set the sample size
  as a factor of the number of components
  in the benchmark.
}
]

See
@secref["gtp-measure-config"
        #:doc '(lib "gtp-measure/scribblings/gtp-measure.scrbl")]
for more details
on some of these options.
