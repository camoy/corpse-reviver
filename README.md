# Corpse Reviver

[![Build Status][build-badge]][build]
[![Scribble][docs-badge]][docs]

## Install

SCV-CR is not available on the Racket package server,
so you must install it from the source.

```
$ git clone https://github.com/camoy/corpse-reviver
$ raco pkg install corpse-reviver/corpse-reviver-artifact \
                   corpse-reviver/corpse-reviver-benchmark \
                   corpse-reviver/corpse-reviver
```

## Use

Invoke SCV-CR using `raco`.
Given a list of paths to typed and untyped files,
SCV-CR will produce optimized bytecode.

```
raco scv-cr server.rkt client.rkt
```

See the [artifact documentation][docs]
for more information.

## Organization

This repository is organized into
several separate packages.

* `corpse-reviver-artifact`
  contains the [artifact documentation][docs].
  This includes instructions for SCV-CR,
  but also the code that generates all of the
  statistics and figures used in the paper
  from the benchmarking data.
* `corpse-reviver-benchmark`
  implements the [command][benchmark]
  that collects data comparing the performance of
  SCV-CR and Typed Racket on a series of benchmarks.
* `corpse-reviver`
  is the code of [SCV-CR][scv-cr] itself.
* `docs` is the rendered artifact documentation
  needed for Github Pages.

build-badge: https://github.com/camoy/corpse-reviver/workflows/build/badge.svg
build: https://github.com/camoy/corpse-reviver/actions?query=workflow%3Abuild
docs-badge: https://img.shields.io/badge/Docs-Scribble-blue.svg
docs: https://camoy.github.io/corpse-reviver
benchmark: https://camoy.github.io/corpse-reviver/corpse-reviver-benchmark.html
scv-cr: https://camoy.github.io/corpse-reviver/corpse-reviver.html
