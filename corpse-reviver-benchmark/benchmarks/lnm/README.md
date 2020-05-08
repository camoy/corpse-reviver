lnm
========

Builds a plot.


The files under `base/` are:

- `data` files containing Racket vectors, representing experimental results
- `module-graphs` TiKZ pictures representing the inheritance hierarchy of a project

History
-------

Original benchmark: <https://github.com/nuprl/gradual-typing-performance>

Differences from [POPL 2016]:

- `main.rkt` increase `NUM_SAMPLES`
- `main.rkt` increase `L-max`
- `main.rkt` increase input size (use `snake` instead of `suffixtree`)
- `main.rkt` do not time parsing
- `spreadsheet.rkt` do not generate spreadsheet
