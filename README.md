# Corpse Reviver

[![Build Status](https://github.com/camoy/corpse-reviver/workflows/build/badge.svg)](https://github.com/camoy/corpse-reviver/actions?query=workflow%3Abuild)
[![Scribble](https://img.shields.io/badge/Docs-Scribble-blue.svg)](https://camoy.github.io/corpse-reviver)

## Install

SCV-CR is not available on the Racket package server,
so you must install it from the source.

```
$ git clone https://github.com/camoy/corpse-reviver
$ raco pkg install corpse-reviver/corpse-reviver-artifact \
                   corpse-reviver/corpse-reviver-benchmark \
                   corpse-reviver/corpse-reviver
```

## Basics

Invoke SCV-CR using `raco`.
Given a list of paths to typed and untyped files,
SCV-CR will produce optimized bytecode.

```
raco scv-cr server.rkt client.rkt
```

## More

See the [artifact documentation](https://camoy.github.io/corpse-reviver) for more information.
