# Last updated: 11.06.2019

####################
### Dependencies ###
####################

#require(expm)
#require(matrixcalc)
#require(mvtnorm)
#require(OpenMx)
#require(matrixStats)


#####################
### Documentation ###
#####################

# fit: a RAM-type OpenMx model
# covariate: covariate used to sort the data. Only a single covariate is
#            implemented.
# level: level of measurement of the covariate. Each level comes with its own
#        methods.
#        - nominal
#        - ordinal
# score_tests: list with different test statistics. The default is
#              list(nominal='LM',ordinal='maxLM',metric='CvM').
#              - DM: double maximum test statistic (ordinal, metric)
#              - CvM: Cramér-von Mises type test statistic (metric)
#              - maxLM: maximum Lagrange multiplier test statistic (ordinal, metric)
#              - LM: Lagrange multiplier (nominal)
# parameter: single or several target parameters. A joint test of all parameters
#            is the default.
# alpha: level of significance. Default is a significance level is alpha = 0.05
#        Currently, only levels of 0.001, 0.005, 0.01, 0.025, 0.05, 0.075, 0.1,
#        0.15, & 0.2 are implemented.


scoretest <- function(fit, covariate, score_tests, parameter = NULL, alpha) {
  
  #####################
  ### Prepare Input ###
  #####################
  
  # Information from the fit object
  data_obs <- as.matrix(fit$data$observed[, fit$manifestVars, drop = FALSE])
  N <- nrow(data_obs)
  p <- length(fit$manifestVars)
  mean_structure <- any(fit$M$free)
  p_star <- p * (p + 1) / 2
  p_star_means <- p * (p + 3) / 2
  q <- length(fit$output$estimate)
  param_names <- names(OpenMx::omxGetParameters(fit))
  exp_cov <- OpenMx::mxGetExpected(model = fit, component = "covariance")
  exp_cov_inv <- solve(exp_cov)
  mean_stru <- any(fit$M$free)
  
  # Check for complete data
  if(!all(complete.cases(cbind(data_obs, covariate)))) {
    stop("Incomplete data detected. Score tests require complete data.")
  }
  
  # Get target parameters
  if (is.null(parameter)) {
    parameter <- param_names
  }
  
  # Number of target parameters
  q_target <- length(parameter)
  
  # Level of measurement and test statistic
  if (!is.factor(covariate)) {
    level <- "metric"
    test <- score_tests["metric"]  # default: CvM
  } else {
    cov_levels <- nlevels(x = covariate)
    if (is.ordered(covariate)) {
      level <- "ordinal"
      test <- score_tests["ordinal"] # default: "maxLM"
    } else {
      level <- "nominal"
      test <- score_tests["nominal"] # default: "LM" 
    }
  }
  
  # Sort the coariate
  covariate_sorted <- covariate[order(covariate)]
  
  # Sort the data by the covariate
  data_obs <- data_obs[order(covariate), ]
  
  # Create output object
  output <- list("Target parameters" = parameter,
                 "Level of measurement" = level,
                 "Score test method" = test)
  
  
  #########################
  ### Individual Scores ###
  #########################
  
  # Calculate Jacobian matrix
  jacobian <- OpenMx::omxManifestModelByParameterJacobian(model = fit)
  if (mean_structure == FALSE) {
    jacobian <- jacobian[1:p_star, ]
  }
  
  # Duplication matrix
  if (p == 1) {
    Dup <- matrix(data = 1)
  } else {
    Dup <- matrixcalc::duplication.matrix(n = p)
  }
  
  # Calculate weight matrix
  V <- 0.5 * t(Dup) %*% kronecker(X = exp_cov_inv, Y = exp_cov_inv) %*% Dup
  if (mean_structure) {
    V_m_cov <- matrix(data = 0, nrow = p_star_means, ncol = p_star_means)
    V_m_cov[1:p_star, 1:p_star] <- V
    V_m_cov[(p_star + 1):p_star_means, (p_star + 1):p_star_means] <- exp_cov_inv
    V <- V_m_cov
  }
  
  # Individual deviations from the sample moments
  cd <- scale(x = data_obs, center = TRUE, scale = FALSE)
  mc <- t(apply(X = cd, MARGIN = 1,
                FUN = function (x){matrixcalc::vech(x %*% t(x))}))
  vech_cov <- matrix(data = rep(x = matrixcalc::vech(exp_cov), times = N),
                     byrow = TRUE, nrow = N, ncol = p_star)
  md <- mc - vech_cov
  if (mean_structure) {
    exp_means <- OpenMx::mxGetExpected(model = fit, component = "means")
    means <- matrix(data = rep(x = exp_means, times = N), byrow = TRUE,
                    nrow = N, ncol = p)
    mean_dev <- data_obs - means
    md <- cbind(md, mean_dev)
  }
  
  # Calculates scores
  scores  <- md %*% V %*% jacobian
  
  # Variance of the individual scores
  fisher <- t(jacobian) %*% V %*% jacobian
  fisher_inv_sqr <- solve(expm::sqrtm(fisher))
  
  
  ######################################
  ### Cumulative Score Process (CSP) ###
  ######################################
  
  n_inv_sqr <- sqrt(1 / N)
  CSP <- n_inv_sqr * matrixStats:::colCumsums(scores) %*% t(fisher_inv_sqr)
  colnames(CSP) <- param_names
  
  # Cumulative scores of target parameters
  CSP_tp <- CSP[, parameter, drop = FALSE]
  
  
  #########################
  ### Nominal Covariate ###
  #########################
  
  if (level == "nominal") {
    if (test == "LM") {
      
      # Cumulative proportions
      cum_prop <- cumsum(table(covariate_sorted)) / N
      
      # Last individual of each bin
      bin_index <- round(N * cum_prop)[1:(cov_levels)]
      
      # Cumulative scores from each group
      CSP_bin <- CSP_tp[bin_index, ]
      
      # Shifted Bins
      CSP_bin_shifted <- rbind(matrix(data = 0, nrow = 1, ncol = q),
                               CSP_bin[-cov_levels, , drop = FALSE])
      
      # Squared deviations 
      SD <- (CSP_bin - CSP_bin_shifted)^2
      
      # Lagrange multiplier test statistic
      LM_test <- sum(SD)
      
      # Parameter with maximum CSP
      LM_par <- parameter[which.max(colMeans(SD))]
      
      # Load simulated critical values
      data("crit_nominal_LM")
      LM_crit_values <- crit_nominal_LM[[paste(cov_levels)]][q_target, ]
      LM_alpha <- as.numeric(colnames(crit_nominal_LM[[paste(cov_levels)]]))
      
      # Interpolate approximated p-value
      LM_p <- stats::approx(x = LM_crit_values, y = LM_alpha, xout = LM_test)
      
      # Test decision
      LM_decision <- LM_p$y < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = LM_test,
                                     "p-value" = LM_p$y,
                                     "H0 rejected" = LM_decision,
                                     "Max parameter" = LM_par,
                                     "Cut point" = NA))
      
    } else {
      
      stop("Error! Use Lagrange multiplier (LM) test for nominal covariates.")
      
    }
  }
  
  #########################
  ### Ordinal Covariate ###
  #########################
  
  if (level == "ordinal") {
    
    # # Levels of the ordinal covariate minus one
    # cov_level <- length(unique(covariate)) - 1
    
    # Cumulative proportions
    cum_prop <- cumsum(table(covariate_sorted)) / N
    
    # Last individual of each bin except the last bin
    bin_index <- round(N * cum_prop)[1:(cov_levels - 1)]
    
    # From the brownian bridge
    weight <- bin_index / N * (1 - bin_index / N) 
    
    # Cumulative proportions associated with the levels of the covariate
    CSP_ord <- CSP_tp[bin_index, , drop = FALSE]
    
    
    #######################
    # Double maximum test #
    #######################
    
    if (test ==  "DM") {
      
      # Double maximum test statistic
      DM_test <- max(weight^(-0.5) * apply(abs(CSP_ord), MARGIN = 1,
                                           FUN = function(x)
                                           {max(x, na.rm = TRUE)}))
      
      # Bin with maximum CSP
      DM_cut <- which.max(weight^(-0.5) *
                            apply(abs(CSP_ord),
                                  MARGIN = 1,FUN = function(x)
                                  {max(x, na.rm = TRUE)}))
      
      # Parameter with maximum CSP
      DM_par <- apply(X = abs(CSP_ord), MARGIN = 1,
                      FUN = which.max)[DM_cut]
      DM_par <- parameter[DM_par]
      
      # Load simulated critical values
      data("crit_ordinal_DM")
      DM_crit_values <- crit_ordinal_DM[[paste(cov_levels)]][q_target, ]
      DM_alpha <- as.numeric(colnames(crit_ordinal_DM[[paste(cov_levels)]]))
      
      # Interpolate approximated p-value
      DM_p <- stats::approx(x = DM_crit_values, y = DM_alpha, xout = DM_test)
      
      # Test decision
      DM_decision <- DM_p$y < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = DM_test,
                                     "p-value" = DM_p$y,
                                     "H0 rejected" = DM_decision,
                                     "Max parameter" = DM_par,
                                     "Cut point" = DM_cut))
      
    }
    
    
    ###################################
    # Maximum Lagrange mutiplier test #
    ###################################
    
    if (test == "maxLM") {
      
      # Weightet CSP bins
      weighted_CSP2 <- weight^(-1) * CSP_ord^2
      
      # Bin sums
      if (dim(weighted_CSP2)[1] == 1) {
        bin_sums <- weighted_CSP2
      } else {
        bin_sums <- rowSums(weighted_CSP2)
      }
      
      # Test statistic
      maxLM_test <- max(bin_sums)
      
      # Bin with maximum CSP
      maxLM_cut <- which.max(bin_sums)
      
      # Parameter with maximum CSP
      maxLM_par <- parameter[which.max(colSums(weighted_CSP2))]
      
      # Load simulated critical values
      data("crit_ordinal_maxLM")
      maxLM_crit_values <- crit_ordinal_maxLM[[paste(cov_levels)]][q_target, ]
      maxLM_alpha <- as.numeric(colnames(crit_ordinal_maxLM[[paste(cov_levels)]]))
      
      # Interpolate approximated p-value
      maxLM_p <- stats::approx(x = maxLM_crit_values, y = maxLM_alpha,
                               xout = maxLM_test)
      
      # Test decision
      maxLM_decision <- maxLM_p$y < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = maxLM_test,
                                     "p-value" = maxLM_p$y,
                                     "H0 rejected" = maxLM_decision,
                                     "Max parameter" = maxLM_par,
                                     "Cut point" = maxLM_cut))
      
    } else{
      
      stop("Error! Use double maximum (DM) test or maximum Lagrange multiplier
           (maxLM) test for ordinal covariates.")
      
    }
  }
  
  
  ########################
  ### Metric Covariate ###
  ########################
  
  if (level == "metric") {
    
    
    #################################
    # Double Maximum Test Statistic #
    #################################
    
    if (test == "DM") {
      
      # Absolute values
      abs_CSP <- abs(CSP_tp)
      
      # Test statistic
      DM_test <- max(abs_CSP)
      
      # Cut point on covariate
      DM_split <- which(abs_CSP == DM_test, arr.ind = TRUE)
      DM_cut <- DM_split[1, 1]
      
      # Parameter with maximum cumulative scores
      DM_par <- parameter[DM_split[1, 2]]
      
      # Approximate p value (100 terms used)
      h <- 1:100
      DM_p <- 1 - (1 + 2 * sum( (-1)^h * exp(-2 * h^2 * DM_test^2)))^q_target
      
      # Test decision
      DM_decision <- DM_p < alpha / 100
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = DM_test,
                                     "p-value" = DM_p,
                                     "H0 rejected" = DM_decision,
                                     "Max parameter" = DM_par,
                                     "Cut point" = DM_cut))
      
    }
    
    
    ###################################
    # Cramér-von Mises Test Statistic #
    ###################################
    
    else if (test == "CvM") {
      
      # Squared CSP
      CSP2 <- CSP_tp^2
      
      # Cram?r-von Mises test statistic
      CvM_test <- 1 / N * sum(CSP2)
      
      # Cut point
      CvM_cut <- which.max(rowSums(CSP2))
      
      # Contributions of individual parameters
      CvM_par <- parameter[which.max(colSums(CSP2))]
      
      # Parameter with maximum CSP
      CvM_max_contrib <- colSums(CSP2)
      
      # Load simulated critical values
      data("crit_metric_CvM")
      CvM_crit_values <- crit_metric_CvM[q_target, ]
      CvM_alpha <- as.numeric(colnames(crit_metric_CvM))
      
      # Interpolate approximated p-value
      CvM_p <- stats::approx(x = CvM_crit_values, y = CvM_alpha,
                             xout = CvM_test)
      
      # Test decision
      CvM_decision <- CvM_p$y < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = CvM_test,
                                     "p-value" = CvM_p$y,
                                     "H0 rejected" = CvM_decision,
                                     "Max parameter" = CvM_par,
                                     "Cut point" = CvM_cut))
      
    }
    
    
    ###################################
    # Maximum Lagrange Test Statistic #
    ###################################
    
    else if (test == "maxLM") {
      
      # Weightet CSP bins
      weighted_CSP2 <- ((1:N) / N * (1 - (1:N) / N))^(-1) * CSP_tp^2
      weighted_CSP2[N, ] <- 0
      
      # Cut lower and upper 10% off
      low_bound <- round(N / 100 * 10)
      upp_bound <- round(N / 100 * 90)
      weighted_CSP2[c(1:low_bound, upp_bound:N), ] <- NA
      
      # Row sums
      row_sums <- rowSums(weighted_CSP2)
      
      # Test statistic
      maxLM_test <- max(row_sums, na.rm = TRUE)
      
      # Cut point
      maxLM_cut <- which.max(row_sums)
      
      # Parameter with maximum CSP
      maxLM_par <- parameter[which.max(colSums(weighted_CSP2, na.rm = TRUE))]
      
      # Load simulated critical values
      data("crit_metric_maxLM")
      maxLM_crit_values <- crit_metric_maxLM[q_target, ]
      maxLM_alpha <- as.numeric(colnames(crit_metric_maxLM))
      
      
      # Interpolate approximated p-value
      maxLM_p <- stats::approx(x = maxLM_crit_values, y = maxLM_alpha,
                               xout = maxLM_test)
      
      # Test decision
      maxLM_decision <- CvM_p$y < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = maxLM_test,
                                     "p-value" = maxLM_p$y,
                                     "H0 rejected" = maxLM_decision,
                                     "Max parameter" = maxLM_par,
                                     "Cut point" = maxLM_cut))
      
    } else {
      
      stop("Error! Use double maximum (DM) test, Cramér von Mises (CvM) test, or
      maximum Lagrange multiplier (maxLM) for metric covariates.")
      
    }
  }
  
  return(output)
}
