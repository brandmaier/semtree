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
# scale: level of measurement of the covariate. Each level comes with its own
#        methods.
#        - nominal
#        - ordinal
#        - metric (default)
# method: Different test statistics. By default all methods suitable for the
#         level of measurement of the covariate are used.
#         - DM: double maximum test statistic (ordinal, metric)
#         - CvM: Cram?r-von Mises type test statistic (metric)
#         - maxLM: maximum Lagrange multiplier test statistic (ordinal, metric)
#         - LM: Lagrange multiplier (nominal)
#         - all: all methods suitable for the level of measurement of the
#           covariate are used (default).
# parameter: single or several target parameters. A joint test of all parameters
#            is the default.
# alpha: level of significance. Default is a significance level of 5%.
#        Currently, only levels of 0.1%, 0.5%, 1%, 2.5%, 5%, 7.5%, 10%, 15%,
#        20%, and 25% are implemented.


scoretest <- function(fit, covariate, scale = "metric", method = "all",
                      parameter = NULL, alpha = .05) {
  
  # compatibility with semtree TODO: fix this somewhere else
  alpha <- alpha*100
  
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
  
  # Get target parameters
  if (is.null(parameter)) {
    parameter <- param_names
  }
  
  # Number of target parameters
  q_target <- length(parameter)
  
  # Sort the coariate
  covariate_sorted <- covariate[order(covariate)]
  
  # Sort the data by the covariate
  data_obs <- data_obs[order(covariate), ]
  
  # Create output object
  output <- c()
  output[length(output) + 1] <- "Structural Change Tests"
  output[length(output) + 1] <- paste("Level of measurement of the covariate:",
                                      scale)
  
  
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
    Dup <-  matrix(data = 1)
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
  CSP <- n_inv_sqr * colCumsums(scores) %*% t(fisher_inv_sqr)
  colnames(CSP) <- param_names
  
  # Cumulative scores of target parameters
  CSP_tp <- CSP[, parameter, drop = FALSE]
  
  
  #########################
  ### Nominal Covariate ###
  #########################
  
  if (scale == "nominal") {
    
    # Levels of covariate minus one
    cov_level <- length(table(covariate_sorted))
    
    # Cumulative proportions
    cum_prop <- cumsum(table(covariate_sorted)) / N
    
    # Last individual of each bin
    bin_index <- round(N * cum_prop)[1:(cov_level)]
    
    # Cumulative scores from each group
    CSP_bin <- CSP_tp[bin_index, ]
    
    # Shifted Bins
    CSP_bin_shifted <- rbind(matrix(data = 0, nrow = 1, ncol = q),
                             CSP_bin[-cov_level, , drop = FALSE])
    
    # Squared deviations 
    SD <- (CSP_bin - CSP_bin_shifted)^2
    
    # Lagrange multiplier test statistic
    LM_test <- sum(SD)
    
    # Parameter with maximum CSP
    LM_par <- parameter[which.max(colMeans(SD))]
    
    # Get critical value
    load("crit_nominal_LM.Rda")
    LM_crit <- crit_nominal_LM[[paste(cov_level)]][q_target, paste(alpha)]
    
    # Test decision
    LM_decision <- LM_test > LM_crit
    
    # Add results to output
    output[length(output) + 1] <- "---"
    output[length(output) + 1] <- "Lagrange multiplier test"
    output[length(output) + 1] <- paste("Parameters under investigation:",
                                        paste(parameter, collapse = ", "))
    output[length(output) + 1] <- paste("Test statistic:", LM_test)
    output[length(output) + 1] <- paste("H0 rejected:", LM_decision)
    output[length(output) + 1] <- paste("Parameter with maximal cumulative scores:",
                                        LM_par)
    output[length(output) + 1] <- paste("Cut point is not available for nominal covariate")
  
  }
  
  
  #########################
  ### Ordinal Covariate ###
  #########################
  
  if (scale == "ordinal") {
    
    # Levels of the ordinal covariate minus one
    cov_level <- length(unique(covariate)) - 1
    
    # Cumulative proportions
    cum_prop <- cumsum(table(covariate_sorted)) / N
    
    # Last individual of each bin
    bin_index <- round(N * cum_prop)[1:(cov_level)]
    
    # From the brownian bridge
    weight <- bin_index / N * (1 - bin_index / N) 
    
    # Cumulative proportions associated with the levels of the covariate
    CSP_ord <- CSP_tp[bin_index, ]
    
    
    #######################
    # Double maximum test #
    #######################
    
    if (method ==  "DM" | method == "all") {
      
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
      
      ### Approximate critical value
      # Covariance matrix of multivariate normal (Merkle et al., 2014)
      Corr <- matrix(data = 0, nrow = cov_level, ncol = cov_level)
      for (i in 1:cov_level) {
        for (j in i:cov_level) {
          Corr[j, i] <- Corr[i, j] <- sqrt(cum_prop[i] * (1 - cum_prop[j])) /
            sqrt(cum_prop[j] * (1 - cum_prop[i]))        
        }
      }
      bonf_cor <- (alpha / 100) / q_target
      DM_crit <- qmvnorm(p = 1 - bonf_cor / 2, mean = rep(0, cov_level),
                         corr = Corr)$quantile
      
      # Test decision
      DM_decision <- DM_test > DM_crit
      
      output[length(output) + 1] <- "---"
      output[length(output) + 1] <- "Double Maximum Test"
      output[length(output) + 1] <- paste("Parameter(s) under investigation:",
                                          paste(parameter, collapse = ", "))
      output[length(output) + 1] <- paste("Test statistic:", DM_test)
      output[length(output) + 1] <- paste("Critical value:", DM_crit)
      output[length(output) + 1] <- paste("H0 rejected:", DM_decision)
      output[length(output) + 1] <- paste("Parameter with maximal cumulative scores:",
                                          DM_par)
      output[length(output) + 1] <- paste("Cut point on covariate:", DM_cut)
    }
    
    
    ###################################
    # Maximum Lagrange mutiplier test #
    ###################################
    
    if (method == "maxLM" | method == "all") {
      
      # Weightet CSP bins
      weighted_CSP2 <- weight^(-1) * CSP_ord^2
      
      # Bin sums
      bin_sums <- rowSums(weighted_CSP2)
      
      # Test statistic
      maxLM_test <- max(bin_sums)
      
      # Bin with maximum CSP
      maxLM_cut <- which.max(bin_sums)
      
      # Parameter with maximum CSP
      maxLM_par <- parameter[which.max(colSums(weighted_CSP2))]
      
      # Directory needs to be set
      load("crit_ordinal_maxLM.Rda")
      maxLM_crit <- crit_ordinal_maxLM[[paste(cov_level + 1)]][q_target, paste(alpha)]
      
      # Test decision
      maxLM_decision <- maxLM_test > maxLM_crit
      
      output[length(output) + 1] <- "---"
      output[length(output) + 1] <- "Maximum Lagrange Multiplier Test"
      output[length(output) + 1] <- paste("Parameter(s) under investigation:",
                                          paste(parameter, collapse = ", "))
      output[length(output) + 1] <- paste("Test statistic:", maxLM_test)
      output[length(output) + 1] <- paste("Critical value:", maxLM_crit)
      output[length(output) + 1] <- paste("H0 rejected:", maxLM_decision)
      output[length(output) + 1] <- paste("Parameter with maximal cumulative scores:",
                                          maxLM_par)
      output[length(output) + 1] <- paste("Cut point on covariate:", maxLM_cut)
    }
  }
  
  
  ########################
  ### Metric Covariate ###
  ########################
  
  if (scale == "metric") {
    
    
    #################################
    # Double Maximum Test Statistic #
    #################################
    
    if (method == "DM" | method == "all") {
      
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
      output[length(output) + 1] <- "---"
      output[length(output) + 1] <- "Double Maximum Test"
      output[length(output) + 1] <- paste("Parameters under investigation:",
                                          paste(parameter, collapse = ", "))
      output[length(output) + 1] <- paste("Test statistic:", DM_test)
      output[length(output) + 1] <- paste("p value:", DM_p)
      output[length(output) + 1] <- paste("H0 rejected:", DM_decision)
      output[length(output) + 1] <- paste("Parameter with maximal cumulative scores:",
                                          DM_par)
      output[length(output) + 1] <- paste("Cut point on covariate:", DM_cut)
    }
    
    
    ###################################
    # Cram?r-von Mises Test Statistic #
    ###################################
    
    if (method == "CvM" | method == "all") {
      
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
      
      # Critical value
      # Directory needs to be set
      load("crit_metric_CvM.Rda")
      CvM_crit <- crit_metric_CvM[q_target, paste(alpha)]
      
      # Test decision
      CvM_decision <- CvM_test > CvM_crit
      
      # Add results to output
      output[length(output) + 1] <- "---"
      output[length(output) + 1] <- "Cram?r-von Mises type test"
      output[length(output) + 1] <- paste("Parameters under investigation:",
                                          paste(parameter, collapse = ", "))
      output[length(output) + 1] <- paste("CvM test statistic:", CvM_test)
      output[length(output) + 1] <- paste("CvM critical value:", CvM_crit)
      output[length(output) + 1] <- paste("H0 rejected:", CvM_decision)
      output[length(output) + 1] <- paste("Parameter with maximal cumulative scores:",
                                          CvM_par)
      output[length(output) + 1] <- paste("Cut point on covariate:", CvM_cut)
      output[length(output) + 1] <- paste("Max contrib:", CvM_max_contrib)
      
    }
    
    
    ###################################
    # Maximum Lagrange Test Statistic #
    ###################################
    
    if (method == "LM" | method == "all") {
      
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
      
      # Approximate critical value
      # Directory needs to be set
      load("crit_metric_maxLM.Rda")
      maxLM_crit <- crit_metric_maxLM[q_target, paste(alpha)]
      
      # Test decision
      maxLM_decision <- maxLM_test > maxLM_crit
      
      output[length(output) + 1] <- "---"
      output[length(output) + 1] <- "Maximum Lagrange Multiplier Test"
      output[length(output) + 1] <- paste("Parameter(s) under investigation:",
                                          paste(parameter, collapse = ", "))
      output[length(output) + 1] <- paste("Test statistic:", maxLM_test)
      output[length(output) + 1] <- paste("Critical value:", maxLM_crit)
      output[length(output) + 1] <- paste("H0 rejected:", maxLM_decision)
      output[length(output) + 1] <- paste("Parameter with maximal cumulative scores:",
                                          maxLM_par)
      output[length(output) + 1] <- paste("Cut point on covariate:", maxLM_cut)
    }
    
  }
  
  
  ##############
  ### Output ###
  ##############
  
  # List with results
  results <- list(output = output,
                  "Target parameters" = parameter,
                  "Number of target parameters" = q_target,
                  "Level of measurement" = scale)
  
  if (method == "LM" | (method == "all" & scale == "nominal")) {
    results <- c(results,
                 LM = list("Test statistic" = LM_test,
                           "Critical value" = LM_crit,
                           "H0 rejected" = LM_decision,
                           "Max parameter" = LM_par,
                           "Cut point" = "not availabe for nominal variables"))
  }
  
  if ( (method == "DM" | method == "all") & scale == "ordinal") {
    results <- c(results,
                 DM = list("Test statistic" = DM_test,
                           "Critical value" = DM_crit,
                           "H0 rejected" = DM_decision,
                           "Max parameter" = DM_par,
                           "Cut point" = DM_cut))
  }
  
  if (method == "maxLM" | (method == "all" & (scale == "metric" | scale == "ordinal"))) {
    results <- c(results,
                 maxLM = list("Test statistic" = maxLM_test,
                              "Critical value" = maxLM_crit,
                              "H0 rejected" = maxLM_decision,
                              "Max parameter" = maxLM_par,
                              "Cut point" = maxLM_cut))
  }
  
  if ( (method == "DM" | method == "all") & scale == "metric") {
    results <- c(results,
                 DM = list("Test statistic" = DM_test,
                           "p-value" = DM_p,
                           "H0 rejected" = DM_decision,
                           "Max parameter" = DM_par,
                           "Cut point" = DM_cut))
  }
  
  if (method == "CvM" | (method == "all" & scale == "metric")) {
    results <- c(results,
                 CvM = list("Test statistic" = CvM_test,
                            "Critical value" = CvM_crit,
                            "H0 rejected" = CvM_decision,
                            "Max parameter"= CvM_par,
                            "Cut point" = CvM_cut))
  }
  
  
  return(results)
}
