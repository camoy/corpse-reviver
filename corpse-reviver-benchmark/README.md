See the [benchmarking documentation][benchmark]
for more information.

## Organization

* `main.rkt` entry point for the benchmarking script.
* `private/config.rkt` defines the configuration options
  set by the command line flags.
* `private/gc.rkt` contains the code for measuring GC details.
* `private/task.rkt` the code that actually runs and measures
  benchmarks.

[benchmark]: https://camoy.github.io/corpse-reviver/corpse-reviver-benchmark.html
