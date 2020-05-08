snake
===



History
---

Original Racket game by David Van Horn.

Original benchmark from: <https://github.com/philnguyen/soft-contract/tree/icfp14/benchmark-contract-overhead>

Original gradual typing benchmark from: <https://github.com/nuprl/gradual-typing-performance>

Differences between this benchmark and [POPL 2016]:

- `main.rkt` parses data outside the timed computation
- `main.rkt` does not reverse the data (its already reversed)
- increase number of iterations, so untyped running time is longer
