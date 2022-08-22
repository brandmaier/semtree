sctest_nominal <- function(cov_sort, scus, min.bucket) {
  
  cov_sort <- droplevels(cov_sort) # drop unused levels
  levels_cov <- levels(cov_sort)
  n_levels <- length(levels_cov)
  
  # check bin.size for nominal covariates with two levels
  if (n_levels == 2) {
    if (any(table(cov_sort) < min.bucket)) {
      return(list(statistic = NA,
                  p.value = 1,
                  cutpoint = NA,
                  par.contrib = NA))
    }
  }
  
  # Get p-value, test statistic and parameter contributions
  CSP <- as.matrix(scus$process)
  CSP <- CSP[-1, , drop = FALSE]
  freq <- proportions(table(cov_sort))
  n <- NROW(CSP)
  d <- diff(rbind(0, CSP[round(cumsum(freq) * n), ]))
  par_contrib <- apply(X = d, MARGIN = 2, FUN = function(x) {sum(x^2 / freq)})
  functional <- strucchange::catL2BB(cov_sort)
  test_statistic <- sum(par_contrib)
  p_value <- functional$computePval(x = test_statistic,
                                    nproc = NCOL(scus$process))
  if (n_levels == 2) {
    cutpoint <- levels_cov[1]
  } else {
    cutpoint = "naive split"
  }
  
  res <- list(statistic = test_statistic,
              p.value = p_value,
              cutpoint = cutpoint,
              par.contrib = par_contrib)
  
}