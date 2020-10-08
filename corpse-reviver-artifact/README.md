See the [artifact documentation][docs]
for more information.

## Organization

* `author_data/` contains the benchark data from the paper.
* `data/` directories that should contain benchmark data
   for generating the figures and statistics.
* `examples/` contains the samples from the artifact documentation.
* `private/diagram.rkt` generates the diagram in the paper
  of how SCV-CR works on typed and untyped modules.
* `private/figure.rkt` creates the rest of the figures (plots and table).
* `private/lattice.rkt` library for rendering lattices.
* `private/read.rkt` consumes the JSON files outputted by the benchmark script.
* `private/stat.rkt` computes statistics from the benchmarking results that
  are reported in the paper.
* `private/util.rkt` utilities for Scribble.

[docs]: https://camoy.github.io/corpse-reviver
