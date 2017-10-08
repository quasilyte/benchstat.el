## Troubleshooting

### Huge run diff

```
name   old time/op    new time/op    delta
Emacs    694µs ±211%    942µs ±255%   ~     (p=0.468 n=11+10)
```

See that `±211%` and `±255%`?  
This is a big problem that makes profile data useless. 

**Solution**

Try to do `(setq lexical-binding nil)`.

This may reduce the scatter effect, but will not
help if you target lexical binding.

Be sure that Emacs process gets enough resources,
otherwise the results will contain more noise.

### Negative execution time with warnings

`benchmark-run` is not very precise, for various reasons,
sometimes it reports negative execution time.

`benchstat.el` does skips these results to avoid profile
data corruption.  
Warnings are emitted to notify you about the problem.

**Solution**

There are two ways to handle this issue.

**(1)** Re-write timed form to make it more computationally expensive.

```elisp
;; Gives negative numbers sometimes.
(benchstat-run :old repetitions (1+ x))

(defmacro repeat (form n)
  (dotimes (_ (1- n))
    (setq form (list (car form) form)))
  form)
  
;; (repeat (1+ x) 3)
;;   => (1+ (1+ (1+ x)))
;; (repeat (1+ x) 5)
;;   => (1+ (1+ (1+ (1+ (1+ x)))))

;; More likely to give reasonable results.
(benchstat-run :old repetitions (repeat (1+ x) 150))
```

It is not always possible to perform such kind of re-writes.
There are occasions where it leads to inaccurate results, 
so you may want to stick to second solution.

**(2)** Increase the total number of runs.

Some runs will be discarded, the others will be recorded.
`benchstat-run-count` variable can be increased in order
to get more runs per `benchmark-run`.

Or you can use multiple invocations of `benchmark-run-more` instead.
