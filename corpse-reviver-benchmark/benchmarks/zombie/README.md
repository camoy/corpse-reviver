zombie
===

Replays a game.
- the game is implemented using functions that simulate OO programming
- the driver module uses type casts to parse input


History
---

The "zombie" game is similar to the Daleks game.

Original game by David Van Horn.

Original benchmark from: <https://github.com/philnguyen/soft-contract/tree/icfp14/benchmark-contract-overhead>

Original gradual typing benchmark from: <https://github.com/nuprl/gradual-typing-performance>

Differences between this benchmark and [POPL 2016]:

- `main.rkt` parses input _outside_ the main computation
- `main.rkt` uses a larger input
- input does not include `to-draw` and `stop-when` commands
