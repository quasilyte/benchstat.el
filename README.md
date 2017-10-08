# benchstat.el

[benchstat](https://godoc.org/golang.org/x/perf/cmd/benchstat) integration that enhances
builtin `benchmark` package with ability to analyze benchmarking results.

## Overview

When you do a performance analysis, or simple benchmarking,
you almost always **compare** two or more functionally equivallent versions of the code.
One of them is expected to be **faster** and/or produce less **allocations**.

Emacs provides useful [benchmarking](https://www.emacswiki.org/emacs/EmacsLispBenchmark) macros
that can be used to get basic profile: execution time and the number of GC runs for it's evaluation.

This package makes proper benchmark results analysis far easier by leveraging [benchstat](https://godoc.org/golang.org/x/perf/cmd/benchstat) utility. 

## Installation

There is no release yet, so you must install it without package manager.

1. Get [benchstat.el](benchstat.el) file.
2. Move `benchstat.el` to the directory where `require` will find it (tip: `load-path` variable).
3. Add `(require 'benchstat)` to your init file (i.e. `~/.emacs`).

> Tip: if you want to experiment without installation, 
> copy/paste [benchstat.el](benchstat.el) file contents into  
> temporary buffer and run `M-x eval-buffer`.

## Quick start

**Case**: your library used `(list x y)` in the places where 2 return values were needed.  
You wounder, how performance may change with transition to `(cons x y)`.

We call `(list x y)` code **old**.  
We call `(cons x y)` code **new**.  
`:old` and `:new` are **profile keys** that specify profile data file to be used.

```elisp
;; Decide how much repetitions is needed.
;; This is the same as `benchmark-run-compiled` REPETITIONS argument.
(defconst repetitions 1000000)

;; Collect old code profile.
(benchstat-run :old repetitions (list 1 2))
;; Collect new code profile.
(benchstat-run :new repetitions (cons 1 2))

;; Display the results.
;; Can be run interactively by `M-x benchstat-compare'.
(benchstat-compare)
```

Evaluation of `benchstat-compare` form creates a temporary buffer which will contain
something like this:

```
name   old time/op    new time/op    delta
Emacs    44.2ms ± 6%    25.0ms ±15%  -43.38%  (p=0.000 n=10+10)

name   old allocs/op  new allocs/op  delta
Emacs      23.0 ± 0%      11.4 ± 5%  -50.43%  (p=0.000 n=10+10)
```

This shows use that:

* `cons` (new) version is almost 2 times faster.
* `cons` (new) version does 2 times less allocations.

> Tip: if you have hard times interpreting the output,
> read [benchstat documentation](https://github.com/golang/perf/tree/master/cmd/benchstat).

`benchstat-run` executes form 10 times by default.
this can be re-defined with `benchstat-run-count` variable:

```elisp
;; Use 5 runs instead of 10.
(let ((benchstat-run-count 5))
  (benchstat-run :old repetitions (cons 1 2)))

;; Additional runs can be performed by `benchstat-run-more',
;; which appends to profile data file.
(benchstat-run-more :old repetitions (cons 1 2)) ;; Additional 10 runs

;; Profile can be reset on demand.
(benchstat-reset :old) ;; Old file had 15 runs info
(benchstat-reset :new)
```

It is possible to define additional **profiles** to make comparison of multiple
implementations easier.  

```elisp
(benchstat-push-profile :vector
                        "/tmp/benchstat-vector")

(benchstat-run :vector repetitions (vector 1 2))

(benchstat-compare :old :vector)
```

When `benchstat-compare` is called with explicit profile keys, single-line comment header
is prepended to the benchstat output.

```
/* old=(:old "/tmp/benchstat-old") new=(:vector "/tmp/benchstat-vector") */

name   old time/op    new time/op    delta
Emacs    44.2ms ± 6%    54.7ms ± 9%  +23.91%  (p=0.000 n=10+10)

name   old allocs/op  new allocs/op  delta
Emacs      23.0 ± 0%      17.0 ± 0%  -26.09%  (p=0.000 n=9+8)
```
