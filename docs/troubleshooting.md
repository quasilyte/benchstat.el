## Troubleshooting

### Negative execution time with warnings

`benchmark-run` is not very precise, for various reasons,
sometimes it reports negative execution time.

`benchstat.el` does skips these results to avoid profile
data corruption. Warnings are emitted to notify you about the problem.

**Solution**

Most of the time, it is sufficient to increase *REPETITIONS*
parameter value, or to add additional computations into
form that is being evaluated.

Other solution is to increase `benchstat-run-count`
variable and get more runs. Some of them should succeed.
