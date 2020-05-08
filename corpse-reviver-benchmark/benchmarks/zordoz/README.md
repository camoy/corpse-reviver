zordoz
===

Traverses and formats bytecode structures

Note `base/compiler-zo-parse.rkt` and `base/compiler-zo-structs.rkt` are from
 the Racket 6.7 release.
These depend on other libraries, but those are more stable and should be
 future-proof.


History
---

Original code: <https://github.com/bennn/zordoz>

Differences between this benchmark and [POPL 2016]:

- `zo-find.rkt` checks for cycles using `eq?` rather than `string=?`
- `main.rkt` parses input bytecode to a zo struct _outside_ the timed computation
- `main.rkt` uses a larger bytecode file as input
