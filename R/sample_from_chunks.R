sample_from_chunks <- function(v, k = 10) {
  stopifnot(is.numeric(v), isTRUE(all.equal(v, sort(v))))
  
  n <- length(v)
  if (k >= n) return(v)
  if (k < 1) stop("k must be positive")
  
  # chunk boundaries (as equal as possible)
  idx <- floor(seq(0, n, length.out = k + 1))
  
  sapply(seq_len(k), function(i) {
    chunk <- v[(idx[i] + 1):idx[i + 1]]
    sample(chunk, 1)
  })
}