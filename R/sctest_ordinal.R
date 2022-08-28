sctest_ordinal <- function(cov_sort, scus, nrep, min.bucket) {
  
  cov_sort <- droplevels(cov_sort) # drop unused levels
  cov_levels <- levels(cov_sort)
  n_levels <- length(cov_levels)
  CSP <- as.matrix(scus$process)
  CSP <- CSP[-1, , drop = FALSE]
  # min.bucket
  tab <- table(cov_sort)
  cum_sum <- cumsum(tab)
  low_pass <- names(which(cum_sum >= min.bucket))
  cum_sum_rev <- cumsum(rev(tab)) 
  high_pass <- names(which(cum_sum_rev > min.bucket))
  passed_levels <- intersect(low_pass, high_pass)
  if (length(passed_levels) <= 1) {
    return(list(statistic = NA,
                p.value = 1,
                cutpoint = NA,
                par.contrib = NA))
  }
  # remove last level of predictor from passed levels
  passed_levels <- intersect(passed_levels, cov_levels[-n_levels])
  freq <- proportions(tab)
  ncat <- length(freq)
  tcat <- cumsum(freq[-ncat])
  n <- NROW(CSP)
  tt <- 1:n / n
  ix <- round(tcat * n)
  CSP <- CSP[ix, , drop = FALSE]
  tt <- tt[ix]
  CSP2 <- CSP^2
  CSP2 <- CSP2 / (tt * (1 - tt))
  rownames(CSP2) <- cov_levels[-n_levels]
  CSP2_passed <- CSP2[passed_levels, , drop = FALSE]
  rows <- rowSums(CSP2_passed)
  names(rows) <- passed_levels
  max_row <- which.max(rows)
  par_contrib <- CSP2[max_row, ]
  test_statistic <- rows[max_row]
  cutpoint <- names(max_row)
  functional <- strucchange::ordL2BB(factor(cov_sort),
                                     nproc = NCOL(scus$process), 
                                     nobs = NULL,
                                     nrep = nrep)
  p_value <- functional$computePval(x = test_statistic,
                                    nproc = NCOL(scus$process))
  
  res <- list(statistic = test_statistic,
              p.value = p_value,
              cutpoint = cutpoint,
              par.contrib = par_contrib,
              btn.matrix = NA)
  
}
