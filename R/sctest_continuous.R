sctest_continuous <- function(cov_sort, scus, from, to, min.bucket) {
  
  # round so that duplicated works as expected
  cov_sort2 <- round(cov_sort, digits = 14)
  
  # increase from and to according to min.bucket
  CSP <- as.matrix(scus$process)
  CSP <- CSP[-1, , drop = FALSE]
  n <- NROW(CSP)
  n1 <- floor(from * n)
  if (n1 < min.bucket) {
    from <- min.bucket / n
    n1 <- min.bucket
  }
  n2 <- floor(to * n)
  if (n - n2 < min.bucket) {
    to <- (n - min.bucket) / n
    n2 <- n - min.bucket
  }
  
  # abort if the sample is too small
  if (from >= to) {
    return(list(statistic = NA,
                p.value = 1,
                cutpoint = NA,
                par.contrib = NA))
  }
  
  ## select subset (considering possible bins with identical values)
  CSP_sub <- CSP[n1:n2, ]
  CSP2_sub <- CSP_sub^2
  tt <- n1:n2 / n
  CSP2_sub <- CSP2_sub / (tt * (1 - tt))
  cov_sort2_sub <- cov_sort2[n1:n2]
  rows <- rowSums(CSP2_sub)
  max_row <- which.max(rows)
  max_cov <- cov_sort2_sub[max_row]
  # check (potential) bin borders of max_row
  tab <- table(cov_sort2)
  freq <- proportions(tab)
  cum_prop <- cumsum(freq)
  # largest accepted unique value of the covariate
  first_outside <- cov_sort2[!duplicated(cov_sort2)][cum_prop > to][1]
  if (max_cov >= first_outside) {
    first_inside <- rev(cov_sort2[!duplicated(cov_sort2)][cum_prop <= to])[1]
    if (is.na(first_inside)) {
      return(list(statistic = NA,
                  p.value = 1,
                  cutpoint = NA,
                  par.contrib = NA))
    }
    n2 <- sum(cov_sort2 <= first_inside)
    to <- n2 / n
    if (from >= to) {
      return(list(statistic = NA,
                  p.value = 1,
                  cutpoint = NA,
                  par.contrib = NA))
    }
    CSP_sub <- CSP[n1:n2, ]
    CSP2_sub <- CSP_sub^2
    tt <- n1:n2 / n
    CSP2_sub <- CSP2_sub / (tt * (1 - tt))
    cov_sort_sub <- cov_sort2[n1:n2]
    rows <- rowSums(CSP2_sub)
    max_row <- which.max(rows)
    max_cov <- cov_sort_sub[max_row]
  } 
  par_contrib <- CSP2_sub[max_row, ]
  test_statistic <- rows[max_row]
  cutpoint <- max_cov
  functional <- strucchange::supLM(from = from, to = to)
  p_value <- functional$computePval(x = test_statistic,
                                    nproc = NCOL(scus$process))
  
  res <- list(statistic = test_statistic,
              p.value = p_value,
              cutpoint = cutpoint,
              par.contrib = par_contrib)
  
}