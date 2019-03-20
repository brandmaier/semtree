# Load packages
#require(expm)
#require(matrixcalc)
#require(OpenMx) 

# fit: a RAM-type OpenMx model
# covariate: covariate used to sort the data. Only a single covariate is
#            implemented.
# method: - DM: double maximum statistic
#         - CvM: Cramér-von Mises type statistic
#         - both: DM & CvM (default)
# parameter: target parameter(s). A joint test of all parameters is the default.
#            CvM can only test up to 20 parameters.
# alpha: approximated significance level for the CvM test. Needs to be between
#        0.01 and 0.2.

scoretest <- function(fit, covariate, method = "both", parameter = "all",
                      alpha = 0.05, min.bucket=7) {
  
  ##############
  ### Set Up ###
  ##############

  # Information from the fit object
  data_obs <- as.matrix(fit$data$observed[, fit$manifestVars, drop = FALSE])
  N <- fit$output$data[[1]][[1]]
  N <- nrow(data_obs)
  p <- length(fit$manifestVars)
  mean_structure <- any(fit$M$free)
  p_star <- p * (p + 1) / 2
  p_star_means <- p * (p + 3) / 2
  q <- length(fit$output$estimate)
  param_names <- names(omxGetParameters(fit))
  exp_cov <- mxGetExpected(model = fit, component = "covariance")
  exp_cov_inv <- solve(exp_cov)
  mean_stru <- any(fit$M$free)
  
  # Get parameters
  if (parameter == "all") {
    parameter <- param_names
  }
  
  # Sort the coariate
  covariate_sorted <- covariate[order(covariate)]
  
  # Sort the data by the covariate
  data_obs <- data_obs[order(covariate), ]
  
  # Duplication matrix
  if (p == 1) {
    Dup <-  matrix(data = 1)
  } else {
    Dup <- duplication.matrix(n = p)
  }
  
  # Calculate Jacobian matrix
  jacobian <- omxManifestModelByParameterJacobian(model = fit)
  if (mean_structure == FALSE) {
    jacobian <- jacobian[1:p_star, ]
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
    exp_means <- mxGetExpected(model = fit, component = "means")
    means <- matrix(data = rep(x = exp_means, times = N), byrow = TRUE,
                    nrow = N, ncol = p)
    mean_dev <- data_obs - means
    md <- cbind(md, mean_dev)
  }
  
  # Calculates scores
  scores  <- md %*% V %*% jacobian
  
  # Variance of the individual scores
  fisher <- t(jacobian) %*% V %*% jacobian
  fisher_inv_sqr <- solve(sqrtm(fisher))
  
  # Create output object
  output <- c()
  
  
  ################################
  ### Cumulative Score Process ###
  ################################
  
  n_inv_sqr <- sqrt(1 / N)
  CSP <- matrix(NA, nrow = N, ncol = q)
  
  for (i in 1:N) {
    CSP[i, ] <- n_inv_sqr * fisher_inv_sqr %*% colSums(scores[1:i, , drop = F])
  }
  colnames(CSP) <- names(omxGetParameters(fit))
  
  # Cumulative scores of target parameters
  CSP_tp <- CSP[, parameter, drop = FALSE]
  
  # Number of target parameters
  q_target <- length(parameter)
  
  
  #####################################
  ### Double Maximum Test Statistic ###
  #####################################
  
  if (method == "DM" | method == "both") {
    
    CSP_tp[c(1:min.bucket, nrow(CSP_tp):(nrow(CSP_tp)-min.bucket)),]<-NA
    
    # Test statistic
    DM <- max(apply(X = CSP_tp, MARGIN = 2, FUN = function(x) 
      {max(abs(x),na.rm = TRUE)}))
    
    # Split point
    max_cov <- covariate_sorted[which(abs(CSP_tp) == DM, arr.ind = TRUE)[1]]
    
    # Parameter with maximum cumulative scores
    max_par <- parameter[which(abs(CSP_tp) == DM, arr.ind = TRUE)[2]]
    
    # Approximate p value (100 terms used)
    h <- 1:100
    DM_p_value <- 1 - (1 + 2 * sum( (-1)^h * exp(-2 * h^2 * DM^2)))^q_target
    
    # Add results to output
    output[length(output) + 1] <- "Double Maximum Test"
    output[length(output) + 1] <- paste("Parameters under investigation:",
                                        paste(parameter, collapse = ", "))
    output[length(output) + 1] <- paste("Parameter with maximal cumulative scores:",
                                        max_par)
    output[length(output) + 1] <- paste("Split point on covariate:", max_cov)
    output[length(output) + 1] <- paste("Test statistic:", DM)
    output[length(output) + 1] <- paste("p value:", DM_p_value)
    
  }
  
  
  #######################################
  ### Cramér-von Mises test statistic ###
  #######################################
  
  if (method == "CvM" | method == "both") {
    
    # Cramér-von Mises test statistic
    CvM <- 1 / N * sum(as.numeric(CSP_tp^2))
    
    # Tables with criticial values from Hansen (1992, page 524)
    crit <- matrix(data = c(0.748, 0.593, 0.470, 0.398, 0.353, 0.243,
                            1.07, 0.898, 0.749, 0.670, 0.610, 0.469,
                            1.35, 1.16, 1.01, 0.913, 0.846, 0.679,
                            1.60, 1.39, 1.24, 1.14, 1.07, 0.883,
                            1.88, 1.63, 1.47, 1.36, 1.28, 1.08,
                            2.12, 1.89, 1.68, 1.58, 1.49, 1.28,
                            2.35, 2.10, 1.90, 1.78, 1.69, 1.46,
                            2.59, 2.33, 2.11, 1.99, 1.89, 1.66,
                            2.82, 2.55, 2.32, 2.19, 2.10, 1.85,
                            3.05, 2.76, 2.54, 2.40, 2.29, 2.03,
                            3.27, 2.99, 2.75, 2.60, 2.49, 2.22,
                            3.51, 3.18, 2.96, 2.81, 2.69, 2.41,
                            3.69, 3.39, 3.15, 3.00, 2.89, 2.59,
                            3.90, 3.60, 3.34, 3.19, 3.08, 2.77,
                            4.07, 3.81, 3.54, 3.38, 3.26, 2.95,
                            4.30, 4.01, 3.75, 3.58, 3.46, 3.14,
                            4.51, 4.21, 3.95, 3.77, 3.64, 3.32,
                            4.73, 4.40, 4.14, 3.96, 3.83, 3.50,
                            4.92, 4.60, 4.33, 4.16, 4.03, 3.69,
                            5.13, 4.79, 4.52, 4.36, 4.22, 3.86),
                   nrow = 20, ncol = 6, byrow = TRUE)
    rownames(crit) <- 1:20
    colnames(crit) <- c(0.01, 0.025, 0.05, 0.075, 0.1, 0.2)
    
    # Critical value
    CvM_crit <- stats::approx(x = c(0.01, 0.025, 0.05, 0.075, 0.1, 0.2),
                              y = crit[q_target, ], xout = alpha)$y
    
    # Test decision
    CvM_decision <- CvM > CvM_crit
    
    # Add results to output
    output[length(output) + 1] <- "---"
    output[length(output) + 1] <- "Cramér-von Mises type test"
    output[length(output) + 1] <- paste("Parameters under investigation:",
                                        paste(parameter, collapse = ", "))
    output[length(output) + 1] <- paste("CvM test statistic:", CvM)
    output[length(output) + 1] <- paste("CvM critical value:", CvM_crit)
    output[length(output) + 1] <- paste("H0 rejected:", CvM_decision)
    
  }
  
  # List with results
  results <- list(output = output,
                  "Target parameters" = parameter,
                  "Number of target parameters" = q_target)
  if (method == "DM" | method == "both") {
    results <- c(results,
                 DM = list(Parameter = max_par,
                           "Split point" = max_cov,
                           "Test statistic" = DM,
                           "p value" = DM_p_value))
  }
  if (method == "CvM" | method == "both") {
    results <- c(results,
                 CvM = list("Test statistic" = CvM,
                            "Critical value" = CvM_crit,
                            "H0 rejected" = CvM_decision))
  }
  
  return(results)
}

