fsm
===


Special Note
---

This benchmark uses a custom `require/typed/check` macro
 (see `both/benchmark-util.rkt`) rather than the one from the
 `require-typed-check` library.

The custom macro handles the `#:opaque` clause differently:
- `require-typed-check` uses `require/typed` to be safe,
- and the custom macro does not use `require/typed`

Safety is not important for this benchmark (we know that the imported
 identifier is a predicate for that struct),
 and being safe causes type errors.

It may be possible to be safe and work around the type errors, but I don't see how.
The solution would need to work for all configurations --- start with a solution
 for `1111`, then try `1110` ... `1011`, and finally try the other configurations.


History
---

Original program: <https://github.com/mfelleisen/sample-fsm>

Original benchmark: <https://github.com/nuprl/gradual-typing-performance>

Differences from original:

- smaller input, so worst case takes ~200 seconds
