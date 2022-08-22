sctest_continuous3 <- function(cov_sort, scus, from, to, min.bucket) {
  
  # identify correct from an to
  
  ## increase from and to according to min.bucket
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
  
  ## select subset (considering possible bins with identical values)
  tab <- table(cov_sort)
  freq <- proportions(tab)
  cumulative_proportions <- cumsum(freq)
  observations_lower_border <- which(cov_sort == as.numeric(names(which(cumulative_proportions >= from)[1])))
  observations_upper_border <- which(cov_sort == as.numeric(names(which(cumulative_proportions >= to)[1])))
  n1_up <- min(observations_lower_border)
  n2_up <- max(observations_upper_border)
  if (n2_up == n) { # if there are no observations left on the right side, remove largest bin
    value_new_upper_border <- as.numeric(names(tab[which(cumulative_proportions >= to)[1]])) - 1
    n2_up <- sum(cov_sort <= value_new_upper_border)
  }
  # change to bins before and after from and to
  from_up <- (n1_up / n)
  to_up <- n2_up / n
  tt <- n1_up:n2_up / n
  tab_subset <- table(cov_sort[n1_up:n2_up])
  freq_subset <- proportions(tab_subset)
  n_cat <- length(freq_subset)
  tcat <- cumsum(freq[-n_cat])
  ix <- round(tcat * n)
  CSP <- CSP[ix, , drop = FALSE]
  tt <- tt[ix]
  CSP2 <- CSP^2
  CSP2 <- CSP2 / (tt * (1 - tt))
  rows <- rowSums(CSP2)
  max_row <- which.max(rows)
  par_contrib <- CSP2[max_row, ]
  test_statistic <- sum(par_contrib)
  cutpoint <- as.numeric(names(max_row))
  functional <- supLM(from = from_up, to = to_up)
  p_value <- functional$computePval(x = test_statistic,
                                    nproc = NCOL(scus$process))
  
  res <- list(statistic = test_statistic,
              p.value = p_value,
              cutpoint = cutpoint,
              par.contrib = par_contrib)
  
  browser()
    
  }