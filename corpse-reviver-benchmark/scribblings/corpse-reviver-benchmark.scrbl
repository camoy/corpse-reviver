#lang scribble/manual

@require[@for-label[corpse-reviver-benchmark
                    racket/base]]
@(define TAG-PREFIX
  "(lib corpse-reviver-benchmark/scribblings/corpse-reviver-benchmark.scrbl)")

@title[#:tag "top" #:tag-prefix TAG-PREFIX]{Benchmark: Corpse Reviver}
@author{Cameron Moy}
@author{Phúc C. Nguyễn}
@author{Sam Tobin-Hochstadt}
@author{David Van Horn}

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
@tabular[#:sep @hspace[3]
         #:row-properties '(top)
         #:style 'boxed
@list[
@list["Short" "Long" "Description"]
@list[
  @exec{-b}
  @exec{--baseline}
  @para{
  Run baseline measurements,
  in other words,
  don't optimize with SCV-CR.
  }]
@list[
  @exec{-c}
  @exec{--cutoff}
  @para{
  Set the maximum number of components
  to measure exhaustively
  before sampling.
  }]
@list[
  @exec{-g}
  @exec{--gc-log}
  @para{
  Log garbage collection statistics
  during analysis.
  This can cause the analysis
  to slow down significantly;
  that's why it's disabled by default.
  }]
@list[
  @exec{-i}
  @exec{--iterations}
  @para{
  Set the number of iterations
  to run for each configuration.
  }]
@list[
  @exec{-n}
  @exec{--no-skip}
  @para{
  Don't skip the analysis of modules
  prefixed with @exec{_}.
  This the convention for indicating
  modules that should be treated as opaque.
  }]
@list[
  @exec{-t}
  @exec{--typed-blame}
  @para{
  By default,
  blame reported by SCV on a typed module is ignored.
  This flag will not ignore those blames.
  }]
@list[
  @exec{-o}
  @exec{--output}
  @para{
  Set the directory
  where analysis results
  will be placed.
  By default,
  it is the current working directory.
  }]
@list[
  @exec{-r}
  @exec{--resume}
  @para{
  Resume an existing setup.
  In other words,
  run the experiment described by
  existing @exec{in} files in
  @exec{$XDG_DATA_HOME/gtp-measure}.
  After running a baseline experiment
  you should use this option
  for the optimized experiment
  so it uses the same samples as the baseline.
  }]
@list[
  @exec{-w}
  @exec{--worker-count}
  @para{
  Set the number of parallel workers
  to use for measuring.
  By default,
  this is @racket[1].
  Each worker is given one benchmark to process
  at a time.
  }]
@list[
  @exec{-R}
  @exec{--num-samples}
  @para{
  Set the number of samples to take
  when measuring a benchmark approximately.
  }]
@list[
  @exec{-S}
  @exec{--sample-factor}
  @para{
  Set the sample size
  as a factor of the number of components
  in the benchmark.
  }]]]

See
@secref["gtp-measure-config"
        #:doc '(lib "gtp-measure/scribblings/gtp-measure.scrbl")]
for more details
on some of these options.
