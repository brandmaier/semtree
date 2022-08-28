sctest_multinomial <- function(covariate, cov_name, min.bucket, model, vcov,
                               scores, sandwich, parm, cur_col, cov_sort,
                               scus) {
  
  # get btn.matrix
  result <- recodeAllSubsets(covariate, cov_name)
  n_obs <- length(covariate)
  n_var <- ncol(result$columns)
  test1 <- c()
  test2 <- rep(NA, times = n_obs)
  
  for (j in 1:n_var) {
    for (i in seq_len(n_obs)) {
      if (isTRUE(result$columns[i, j])) {
        test1[i] <- 1
      }
      else if (!is.na(result$columns[i, j])) {
        test1[i] <- 0
      }
      else{
        test1[i] <- NA
      }
    }
    test1 <- as.factor(test1)
    test2 <- data.frame(test2, test1)
  }
  test2 <- test2[, -1]
  
  LL.within <- NULL
  within.split <- NULL
  
  # check if minimal group sizes are reached
  passed_min_bucked <- apply(
    apply(test2, MARGIN = 2, FUN = table) >= min.bucket, MARGIN = 2, FUN = all)
  
  for (i in seq_len(n_var)) {
    if (passed_min_bucked[i]) {
    cov_temp <- test2[, i]
    index_temp <- order(cov_temp)
    scores_sorted <- scores[index_temp, , drop = FALSE]
    cov_temp_sort <- cov_temp[index_temp]
    scus_temp <- gefp_semtree(x = model, order.by = cov_temp_sort, vcov = vcov,
                         scores = scores_sorted, decorrelate = TRUE,
                         sandwich = sandwich, parm = parm, cur_col = cur_col)
    functional_temp <- strucchange::catL2BB(cov_temp)
    test_statistic_temp <- functional_temp$computeStatistic(scus_temp$process)
    } else {
      test_statistic_temp <- 0
    }
    LL.within <- c(LL.within, test_statistic_temp)
    within.split <- c(within.split, i)
  }
  
  btn.matrix <- rbind(LL.within,
                      rep(cov_name, times = n_var),
                      rep(cur_col, times = n_var),
                      within.split)
  colnames(btn.matrix) <- c(paste("var", seq(1, n_var), sep = ""))
  rownames(btn.matrix) <- c("LR", "variable", "column", "split val")
  
  
  # ordinary lm test
  cov_sort <- droplevels(cov_sort) # drop unused levels
  levels_cov <- levels(cov_sort)
  n_levels <- length(levels_cov)
  
  # check bin.size if any partition reached min.bucket
    if (all(!passed_min_bucked)) {
      return(list(statistic = NA,
                  p.value = 1,
                  cutpoint = NA,
                  par.contrib = NA,
                  btn.matrix = NA))
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
  max_LL.within <- which.max(LL.within)
  
  res <- list(statistic = test_statistic,
              p.value = p_value,
              cutpoint = btn.matrix["split val", max_LL.within],
              par.contrib = par_contrib,
              btn.matrix = btn.matrix,
              best_partition = test2[, max_LL.within] == 1)
  
}
