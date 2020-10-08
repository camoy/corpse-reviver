See the [SCV-CR documentation][scv-cr]
for more information.

## Organization

* `main.rkt` entry point for SCV-CR.
* `opaque.rkt` a library that provides forms that mark
  dependencies as opaque imports.
* `require-typed-check.rkt` used in benchmarks instead of
  the require-typed-check library.
* `private/compile.rkt` utilities for expansion and bytecode compilation.
* `private/data.rkt` defines all the structs used in the
  other modules.
* `private/dependency.rkt` provides functions for locating
  the contracts that need to be kept given source location information
  from SCV analysis results.
* `private/elaborate.rkt` converts a Typed Racket module into a
  Racket module with explicit contracts attached for verification.
* `private/extract.rkt` takes a Typed Racket file and gets all of the
  information about the generated contracts.
* `private/logging.rkt` used for logging debug events as well as
  communicating information to the benchmarking script.
* `private/munge.rkt` modifies contracts so they can be handled by SCV.
* `private/optimize.rkt` calls SCV to analyze a program and uses this
  result to soundly optimize.
* `private/struct.rkt` provides utilities for handling structs
  in Typed Racket programs. They need special handling.
* `private/syntax.rkt` utilities for dealing with syntax objects and scopes.
* `private/typed-racket.rkt` a library imported by the Typed Racket patches.
* `private/util.rkt` defines basic utilities used across all of SCV-CR.
* `private/lang/` implements several languages used to support analysis
  and optimization.
* `private/lib-typed-racket/` contains modified versions of some modules
  of the Typed Racket implementation that can intercept contracts and
  other information.

scv-cr: https://camoy.github.io/corpse-reviver/corpse-reviver.html
