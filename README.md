# Corpse Reviver

[![Build Status](https://github.com/camoy/corpse-reviver/workflows/build/badge.svg)](https://github.com/camoy/corpse-reviver/actions?query=workflow%3Abuild)

## Install

SCV-CR is not available on the Racket package server, so you must install it from the source.

```
git clone https://github.com/camoy/corpse-reviver
raco pkg install corpse-reviver/*/
```

## Run

You can invoke SCV-CR using `raco`. Given a list of paths to typed and untyped files, SCV-CR will produce optimized bytecode.

```
raco scv-cr server.rkt client.rkt
```

## Benchmark

A benchmark script, using all the same parameters as described in the paper, is executed with one command. It outputs the benchmark results as JSON files in the current directory.

```
raco scv-cr-benchmark
```
