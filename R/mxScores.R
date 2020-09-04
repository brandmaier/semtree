mxScores <- function(x, control) {
  p <- control$scores_info$p
  mean_structure <- control$scores_info$mean_structure
  p_star <- control$scores_info$p_star
  p_star_seq <- seq_len(p_star)
  p_star_means <- control$scores_info$p_star_means
  
  exp_cov <- OpenMx::mxGetExpected(model = x, component = "covariance")
  exp_cov_inv <- solve(exp_cov)
  data_obs <- x$data$observed[, x$manifestVars, drop = FALSE]
  N <- nrow(data_obs)
  
  if (control$linear) {
    
    q <- control$scores_info$q
    q_seq <- control$scores_info$q_seq
    p_unf <- control$scores_info$p_unf
    
    A_deriv <- control$scores_info$A_deriv
    S_deriv <- control$scores_info$S_deriv
    m_deriv <- control$scores_info$m_deriv
    
    F_RAM <- x$F$values
    m <- t(x$M$values)
    B <- solve(diag(x = 1, nrow = NROW(x$A$values)) - x$A$values)
    E <- B %*% x$S$values %*% t(B)
    FB <- F_RAM %*% B
    
    jac <- matrix(0, nrow = p_star_means, ncol = q)
    
    for (i in seq_len(q)) {
      symm <- FB %*% A_deriv[[i]] %*% E %*% t(F_RAM)
      jac[p_star_seq, i] <- lavaan::lav_matrix_vech(symm + t(symm) + FB %*% S_deriv[[i]] %*% t(FB))
    }
    
    for (i in seq_len(q)) {
      jac[(p_star+1):p_star_means, i] <- FB %*% A_deriv[[i]] %*% B %*% m +
        FB %*% m_deriv[[i]]
    }
    
    colnames(jac) <- names(x$output$estimate)
    
  } else {
    
    jac <- OpenMx::omxManifestModelByParameterJacobian(model = x)
  
    }
    
  if (mean_structure == FALSE) {jac <- jac[p_star_seq, ]}
  
  # Calculate weight matrix
  Dup <- lavaan::lav_matrix_duplication(n = p)
  V <- 0.5 * t(Dup) %*% kronecker(X = exp_cov_inv, Y = exp_cov_inv) %*% Dup
  if (mean_structure) {
    V_m_cov <- matrix(data = 0, nrow = p_star_means, ncol = p_star_means)
    V_m_cov[p_star_seq, p_star_seq] <- V
    V_m_cov[(p_star + 1):p_star_means, (p_star + 1):p_star_means] <- exp_cov_inv
    V <- V_m_cov
  }
  
  # Individual deviations from the sample moments
  cd <- scale(x = data_obs, center = TRUE, scale = FALSE)
  if (p == 1) {
    mc <- matrix(apply(X = cd, MARGIN = 1,
                FUN = function(x) {lavaan::lav_matrix_vech(x %*% t(x))}))
  } else {
    mc <- t(apply(X = cd, MARGIN = 1,
                  FUN = function(x) {lavaan::lav_matrix_vech(x %*% t(x))}))
  }
  vech_cov <- matrix(data = rep(x = lavaan::lav_matrix_vech(exp_cov), times = N),
                     byrow = TRUE, nrow = N, ncol = p_star)
  md <- mc - vech_cov
  if (mean_structure) {
    exp_means <- OpenMx::mxGetExpected(model = x, component = "means")
    means <- matrix(data = rep(x = exp_means, times = N), byrow = TRUE,
                    nrow = N, ncol = p)
    mean_dev <- data_obs - means
    md <- as.matrix(cbind(md, mean_dev))
  }
  
  # Calculates scores
  scores  <- md %*% V %*% jac
  
  return(scores)
  
}
