# Memory efficiency opportunities in `varimp()`

This note summarizes three patch ideas for making `varimp()` and its helpers less memory-intensive.

1. **Reuse a single permuted data frame inside `varimpTree()`**
   * Current code clones the full OOB data for every predictor: `oob.data.permuted <- oob.data` inside the variable loop, which duplicates the data frame up to `length(var.names)` times per tree. A patch could allocate `oob.data.permuted <- oob.data` once before the loop and, for each predictor, replace only the affected column in place (temporarily storing the original column to restore it after evaluation). That change would avoid repeated whole-frame copies while still keeping the evaluation semantics intact.
2. **Preallocate outputs instead of building a large intermediate list**
   * `varimp()` currently collects per-tree results into a `temp` list and then materializes both `ll.baselines` and `importance` via multiple `sapply()` calls, effectively holding three copies of the same numeric payload. Preallocating the `ll.baselines` vector and `importance` matrix up front and filling them as each future returns (e.g., using an index-aware map) would keep only one copy in memory and eliminate the extra list-to-matrix transposes.
3. **Optional aggregated return to skip the per-tree matrix**
   * The function always stores the full `k x p` importance matrix, even when callers only need aggregate measures. Introducing an argument such as `aggregate = c("none", "mean")` and accumulating a running mean (and, optionally, variance) during the per-tree loop would let users drop the per-tree matrix entirely when it is not needed, substantially reducing the memory footprint for large forests.
