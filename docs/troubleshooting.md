## Troubleshooting

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

;; More likely to gives reasonable results.
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
