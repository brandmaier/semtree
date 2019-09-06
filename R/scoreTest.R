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
#              list(nominal = 'LM', ordinal = 'maxLM', metric = 'CvM').
#              - DM: double maximum test statistic (ordinal, metric)
#              - CvM: Cramér-von Mises type test statistic (metric)
#              - maxLM: maximum Lagrange multiplier test statistic (ordinal, metric)
#              - LM: Lagrange multiplier (nominal)
# parameter: single or several target parameters. A joint test of all parameters
#            is the default.
# alpha: level of significance. Default is a significance level is alpha = 0.05
#        Currently, only levels of 0.001, 0.005, 0.01, 0.025, 0.05, 0.075, 0.1,
#        0.15, & 0.2 are implemented.


scoretest <- function(fit, data_sorted, covariate_sorted, level, test,
                      parameter = NULL, alpha, min_bucket, bin_control) {
  
  #####################
  ### Prepare Input ###
  #####################
  
  # Information from the fit object
  N <- nrow(data_sorted)
  p <- length(fit$manifestVars)
  mean_structure <- any(fit$M$free)
  p_star <- p * (p + 1) / 2
  p_star_means <- p * (p + 3) / 2
  q <- length(fit$output$estimate)
  param_names <- names(OpenMx::omxGetParameters(fit))
  exp_cov <- OpenMx::mxGetExpected(model = fit, component = "covariance")
  exp_cov_inv <- solve(exp_cov)
  mean_stru <- any(fit$M$free)
  cov_level <- 0 # This prevents an error
  bin_control$small_bin <- FALSE # reset this value
  
  # Get target parameters
  if (is.null(parameter)) {
    parameter <- param_names
  }
  
  # Number of target parameters
  q_target <- length(parameter)
  
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
  cd <- scale(x = data_sorted, center = TRUE, scale = FALSE)
  mc <- t(apply(X = cd, MARGIN = 1,
                FUN = function (x){matrixcalc::vech(x %*% t(x))}))
  vech_cov <- matrix(data = rep(x = matrixcalc::vech(exp_cov), times = N),
                     byrow = TRUE, nrow = N, ncol = p_star)
  md <- mc - vech_cov
  if (mean_structure) {
    exp_means <- OpenMx::mxGetExpected(model = fit, component = "means")
    means <- matrix(data = rep(x = exp_means, times = N), byrow = TRUE,
                    nrow = N, ncol = p)
    mean_dev <- data_sorted - means
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
      
      # Levels of covariate
      cov_levels <- nlevels(covariate_sorted)
      
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
      
      # Contributions of each parameter
      LM_contrib <- SD
      
      # Cut point for seemtree (only for nominal covariate with 2 levels)
      if (cov_levels == 2) {
        max_obs <- bin_index[1]
        if (all(!is.na(suppressWarnings(as.numeric(levels(x = covariate_sorted)))))) {
          cut_point <- sum(as.numeric(levels(x = covariate_sorted)))/ 2
        } else {
          cut_point <- 0.5
        } 
      } else {
        max_obs <- NA
        cut_point <- NA
      }
      
      # Lagrange multiplier test statistic
      LM_test <- sum(SD)
      
      # Parameter with maximum CSP
      LM_max_par <- parameter[which.max(colMeans(SD))]
      
      # Approximate p-value (only for interval 0.25 >= p >= 0.001)
      data("crit_nominal_LM")
      LM_crit_values <- crit_nominal_LM[[paste(cov_levels)]][q_target, ]
      if (LM_test > max(LM_crit_values)) {
        LM_p <- 0.001
        LM_p_region <- "< 0.001"
      } else if (LM_test < min(LM_crit_values)) {
        LM_p <- 0.25
        LM_p_region <- "> 0.25"
      } else {
        LM_alpha <- as.numeric(colnames(crit_nominal_LM[[paste(cov_levels)]]))
        LM_p <- stats::approx(x = LM_crit_values, y = LM_alpha, xout = LM_test)$y
        LM_p_region <- "inside"
      }
      
      # Test decision
      LM_decision <- LM_p < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = LM_test,
                                     "p-value" = LM_p,
                                     "p-value region" = LM_p_region,
                                     "H0 rejected" = LM_decision,
                                     "Max parameter" = LM_max_par,
                                     "Parameter contribution" = LM_contrib,
                                     "Cut point" = cut_point,
                                     "Obs before cut" = max_obs))
      
    } else {
      
      stop("Error! Use Lagrange multiplier (LM) test for nominal covariates.")
      
    }
  }
  
  #########################
  ### Ordinal Covariate ###
  #########################
  
  if (level == "ordinal") {
    
    # Levels of covariate
    cov_levels <- nlevels(covariate_sorted)
    
    # Name of the levels of the covariate
    if (all(!is.na(suppressWarnings(as.numeric(levels(x = covariate_sorted)))))) {
      ordinal_levels <- as.numeric(levels(x = covariate_sorted))
    } else {
      ordinal_levels <- 1:nlevels(x = covariate_sorted)
    }
    
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
      
      # Absolute values
      abs_CSP_ord <- abs(CSP_ord)
      
      # Contributions of each parameter
      DM_contrib <- apply(X = weight^(-0.5) * abs_CSP_ord, MARGIN = 2,
                          FUN = max)
      
      # Double maximum test statistic
      DM_test <- max(weight^(-0.5) * apply(abs_CSP_ord, MARGIN = 1,
                                           FUN = function(x)
                                           {max(x, na.rm = TRUE)}))
      
      # Cut point for seemtree
      max_obs <- which.max(weight^(-0.5) * apply(abs_CSP_ord,
                                                 MARGIN = 1,FUN = function(x)
                                                 {max(x, na.rm = TRUE)}))
      cut_point <- (ordinal_levels[max_obs] +
                      ordinal_levels[max_obs + 1]) / 2 
      
      # Parameter with maximum CSP
      DM_max_par <- apply(X = abs_CSP_ord, MARGIN = 1,
                          FUN = which.max)[max_obs]
      DM_max_par <- parameter[DM_max_par]
      
      # Approximate p-value (only for interval 0.25 >= p >= 0.001)
      data("crit_ordinal_DM")
      DM_crit_values <- crit_ordinal_DM[[paste(cov_levels)]][q_target, ]
      if (DM_test > max(DM_crit_values)) {
        DM_p <- 0.001
        DM_p_region <- "< 0.001"
      } else if (DM_test < min(DM_crit_values)) {
        DM_p <- 0.25
        DM_p_region <- "> 0.25"
      } else {
        DM_alpha <- as.numeric(colnames(crit_ordinal_DM[[paste(cov_levels)]]))
        DM_p <- stats::approx(x = DM_crit_values, y = DM_alpha, xout = DM_test)$y
        DM_p_region <- "inside"
      }
      
      # Test decision
      DM_decision <- DM_p < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = DM_test,
                                     "p-value" = DM_p,
                                     "p-value region" = DM_p_region,
                                     "H0 rejected" = DM_decision,
                                     "Max parameter" = DM_max_par,
                                     "Parameter contribution" = DM_contrib,
                                     "Cut point" = cut_point,
                                     "Obs before cut" = max_obs))
      
    }
    
    
    ###################################
    # Maximum Lagrange mutiplier test #
    ###################################
    
    if (test == "maxLM") {
      
      # Weightet CSP bins
      weighted_CSP2 <- weight^(-1) * CSP_ord^2
      
      # Contributions of each parameter
      maxLM_contrib <- apply(X = weighted_CSP2, MARGIN = 2, FUN = max)
      
      # Bin sums
      if (dim(weighted_CSP2)[1] == 1) {
        bin_sums <- weighted_CSP2
      } else {
        bin_sums <- rowSums(weighted_CSP2)
      }
      
      # Test statistic
      maxLM_test <- max(bin_sums)
      
      # Cut point for seemtree
      max_obs <- which.max(bin_sums)
      cut_point <- (ordinal_levels[max_obs] +
                      ordinal_levels[max_obs + 1]) / 2 
      
      # Parameter with maximum CSP
      maxLM_max_par <- parameter[which.max(colSums(weighted_CSP2))]
      
      # Approximate p-value (only for interval 0.25 >= p >= 0.001)
      data("crit_ordinal_maxLM")
      maxLM_crit_values <- crit_ordinal_maxLM[[paste(cov_levels)]][q_target, ]
      if (maxLM_test > max(maxLM_crit_values)) {
        maxLM_p <- 0.001
        maxLM_p_region <- "< 0.001"
      } else if (maxLM_test < min(maxLM_crit_values)) {
        maxLM_p <- 0.25
        maxLM_p_region <- "> 0.25"
      } else {
        maxLM_alpha <- as.numeric(colnames(crit_ordinal_maxLM[[paste(cov_levels)]]))
        maxLM_p <- stats::approx(x = maxLM_crit_values, y = maxLM_alpha,
                                 xout = maxLM_test)$y
        maxLM_p_region <- "inside"
      }
      
      # Test decision
      maxLM_decision <- maxLM_p < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = maxLM_test,
                                     "p-value" = maxLM_p,
                                     "p-value region" = maxLM_p_region,
                                     "H0 rejected" = maxLM_decision,
                                     "Max parameter" = maxLM_max_par,
                                     "Parameter contribution" = maxLM_contrib,
                                     "Cut point" = cut_point,
                                     "Obs before cut" = max_obs))
      
    } else {
      
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
      
      # Contributions of each parameter
      DM_contrib <- apply(X = abs_CSP, MARGIN = 2, FUN = max)
      
      # Test statistic
      DM_test <- max(abs_CSP)
      
      # Cut point for seemtree
      max_obs_par <- which(abs_CSP == DM_test, arr.ind = TRUE)
      max_obs <- max_obs_par[1, 1]
      cut_point <- (covariate_sorted[max_obs] +
                      covariate_sorted[max_obs + 1]) / 2
      
      # Parameter with maximum cumulative scores
      DM_max_par <- parameter[max_obs_par[1, 2]]
      
      # Approximate p value (100 terms used)
      h <- 1:100
      DM_p <- 1 - (1 + 2 * sum( (-1)^h * exp(-2 * h^2 * DM_test^2)))^q_target
      
      # Test decision
      DM_decision <- DM_p < alpha / 100
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = DM_test,
                                     "p-value" = DM_p,
                                     "p-value region" = "inside",
                                     "H0 rejected" = DM_decision,
                                     "Max parameter" = DM_max_par,
                                     "Parameter contribution" = DM_contrib,
                                     "Cut point" = cut_point,
                                     "Obs before cut" = max_obs))
      
    }
    
    
    ###################################
    # Cramér-von Mises Test Statistic #
    ###################################
    
    else if (test == "CvM") {
      
      # Squared CSP
      CSP2 <- CSP_tp^2
      
      # Contributions of each parameter
      CvM_contrib <- colMeans(x = CSP2)
      
      # Cramér-von Mises test statistic
      CvM_test <- 1 / N * sum(CSP2)
      
      # Cut point for seemtree
      max_obs <- which.max(rowSums(CSP2))
      cut_point <- (covariate_sorted[max_obs] +
                      covariate_sorted[max_obs + 1]) / 2
      
      # Contributions of individual parameters
      CvM_max_par <- parameter[which.max(colSums(CSP2))]
      
      # Parameter with maximum CSP
      CvM_max_contrib <- colSums(CSP2)
      
      # Approximate p-value (only for interval 0.25 >= p >= 0.001)
      data("crit_metric_CvM")
      CvM_crit_values <- crit_metric_CvM[q_target, ]
      if (CvM_test > max(CvM_crit_values)) {
        CvM_p <- 0.001
        CvM_p_region <- "< 0.001"
      } else if (CvM_test < min(CvM_crit_values)) {
        CvM_p <- 0.25
        CvM_p_region <- "> 0.25"
      } else {
        CvM_alpha <- as.numeric(colnames(crit_metric_CvM))
        CvM_p <- stats::approx(x = CvM_crit_values, y = CvM_alpha,
                               xout = CvM_test)$y
        CvM_p_region <- "inside"
      }
      
      # Test decision
      CvM_decision <- CvM_p < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = CvM_test,
                                     "p-value" = CvM_p,
                                     "p-value region" = CvM_p_region,
                                     "H0 rejected" = CvM_decision,
                                     "Max parameter" = CvM_max_par,
                                     "Parameter contribution" = CvM_contrib,
                                     "Cut point" = cut_point,
                                     "Obs before cut" = max_obs))
      
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
      
      # Contributions of each parameter
      maxLM_contrib <- apply(X = weighted_CSP2, MARGIN = 2, FUN = max, 
                             na.rm = TRUE)
      
      # Row sums
      row_sums <- rowSums(weighted_CSP2)
      
      # Test statistic
      maxLM_test <- max(row_sums, na.rm = TRUE)
      
      # Cut point for seemtree
      max_obs <- which.max(row_sums)
      cut_point <- (covariate_sorted[max_obs] +
                      covariate_sorted[max_obs + 1]) / 2
      
      # Parameter with maximum CSP
      maxLM_max_par <- parameter[which.max(colSums(weighted_CSP2, na.rm = TRUE))]
      
      # Approximate p-value (only for interval 0.25 >= p >= 0.001)
      data("crit_metric_maxLM")
      maxLM_crit_values <- crit_metric_maxLM[q_target, ]
      if (maxLM_test > max(maxLM_crit_values)) {
        maxLM_p <- 0.001
        maxLM_p_region <- "< 0.001"
      } else if (maxLM_test < min(maxLM_crit_values)) {
        maxLM_p <- 0.25
        maxLM_p_region <- "> 0.25"
      } else {
        maxLM_alpha <- as.numeric(colnames(crit_metric_maxLM))
        maxLM_p <- stats::approx(x = maxLM_crit_values, y = maxLM_alpha,
                                 xout = maxLM_test)$y
        maxLM_p_region <- "inside"
      }
      
      # Test decision
      maxLM_decision <- maxLM_p < alpha
      
      # Add results to output
      output <- append(x = output,
                       values = list("Test statistic" = maxLM_test,
                                     "p-value" = maxLM_p,
                                     "p-value region" = maxLM_p_region,
                                     "H0 rejected" = maxLM_decision,
                                     "Max parameter" = maxLM_max_par,
                                     "Parameter contribution" = maxLM_contrib,
                                     "Cut point" = cut_point,
                                     "Obs before cut" = max_obs))
      
    } else {
      
      stop("Error! Use double maximum (DM) test, Cramér von Mises (CvM) test, or
      maximum Lagrange multiplier (maxLM) for metric covariates.")
      
    }
  }
  
  
  #####################################
  ### Check if Groups are too Small ###
  #####################################
  
  # Filter out nominal covariate with more than two levels
  if ( (level == "nominal" & cov_level == 2) | level == "ordinal" |
       level == "metric") {
    
    # Number of observations until cut point
    left_obs <- max_obs
    right_obs <- N - max_obs
    
    # Check left side
    if (left_obs < min_bucket & bin_control$censored_left == FALSE) {bin_control$small_bin <- "left"}
    if (right_obs < min_bucket & bin_control$censored_right == FALSE) {bin_control$small_bin <- "right"}
  }
  
  # Add info about sparseness to output
  output <- c(output, bin_control = list(bin_control))
  
  
  return(output)
}
