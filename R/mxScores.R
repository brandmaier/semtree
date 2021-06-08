mxScores <- function(x, control) {
  if (OpenMx::imxHasDefinitionVariable(x)) {
    return(mxScores_df(x = x, control = control))
  } else {
    return(mxScores_standard(x = x, control = control))
  }
}


mxScores_standard <- function(x, control) {
  
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
    
  if (mean_structure == FALSE) {jac <- jac[p_star_seq, , drop = FALSE]}
  
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


mxScores_df <- function(x, control) {
  
  p <- control$scores_info$p
  mean_structure <- control$scores_info$mean_structure
  p_star <- control$scores_info$p_star
  p_star_seq <- seq_len(p_star)
  p_star_means <- control$scores_info$p_star_means
  p_star_p_means_seq <- (p_star + 1):p_star_means
  p_unf <- control$scores_info$p_unf
  
  data_obs <- x$data$observed[, x$manifestVars, drop = FALSE]
  N <- nrow(data_obs)
  Ident <- diag(x = 1, nrow = p_unf)
  Dup <- lavaan::lav_matrix_duplication(n = p)
  
  # Scores
  scores <- matrix(NA, nrow = N, ncol = control$scores_info$q)
  colnames(scores) <- names(x$output$estimate)
  
  # Individual sample moments
  cd <- scale(x = data_obs, center = TRUE, scale = FALSE)
  if (p == 1) {
    mc <- matrix(apply(X = cd, MARGIN = 1,
                       FUN = function(x) {lavaan::lav_matrix_vech(x %*% t(x))}))
  } else {
    mc <- t(apply(X = cd, MARGIN = 1,
                  FUN = function(x) {lavaan::lav_matrix_vech(x %*% t(x))}))
  }
  
  # Prepare empty object for the individual deviations from the sample moments
  if (mean_structure) {
    md <- matrix(NA, nrow = N, ncol = p_star_means)
  } else {
    md <- matrix(NA, nrow = N, ncol = p_star)
  }
  
  # Get definition variables
  df <- identify_definition_variables(x)
  df_labels <- df[[1]]
  df_nr <- length(df_labels)
  df_data <- x$data$observed[, df[[2]], drop = FALSE]
  df_indices <- seq_along(df_labels)
  
  ## RAM matrices
  F_RAM <- x$F$values
  A <- x$A$values
  S <- x$S$values
  m <- t(x$M$values)
  B <- solve(Ident - A)
  FB <- F_RAM %*% B
  E <- B %*% S %*% t(B)
  
  if (control$linear) {
    
    q <- control$scores_info$q
    q_seq <- control$scores_info$q_seq
    
    A_deriv <- control$scores_info$A_deriv
    S_deriv <- control$scores_info$S_deriv
    m_deriv <- control$scores_info$m_deriv
    
    jac <- matrix(0, nrow = p_star_means, ncol = q)
    
  }
  
  ## Assign definition variables to the corresponding RAM matrices
  RAM_df <- rep(NA, times = df_nr)
  RAM_df[which(df_labels %in% x$A$labels)] <- "A"
  RAM_df[which(df_labels %in% x$S$labels)] <- "S"
  RAM_df[which(df_labels %in% x$M$labels)] <- "M"
  
  # Get coordinates of the definition variables in the corresponding RAM matrices
  RAM_coord <- list()
  for (j in df_indices) {
    if (RAM_df[j] == "A") {
      RAM_coord[[j]] <- which(x$A$labels == df_labels[j], arr.ind = TRUE)
    }
    if (RAM_df[j] == "S") {
      RAM_coord[[j]] <- which(x$S$labels == df_labels[j], arr.ind = TRUE)
    }
    if (RAM_df[j] == "M") {
      RAM_coord[[j]] <- which(t(x$M$labels) == df_labels[j], arr.ind = TRUE)
    }
  }
  
  # Get groups of individuals with identical definition variables
  group <- transform(df_data,
                     group_ID = as.numeric(interaction(df_data,
                                                       drop = TRUE)))
  unique_groups <- unique(group$group_ID)
  
  # Loop over the all the different definition variable values
  for (i in unique_groups) { # Start loop with index i
    
    group_rows <- which(group$group_ID == i) # CHECK IF NEEDED
    group_n <- NROW(group_rows)
    df_values <- as.numeric(group[group$group_ID == i, ][1, ])
    
    # Update the definition variable values in the RAM matrices
    for (j in df_indices){
      
      if (RAM_df[j] == "A") {
        A[RAM_coord[[j]]] <- df_values[j]
      }
      
      if (RAM_df[j] == "S") {
        S[RAM_coord[[j]]] <- df_values[j]
      }
      
      if (RAM_df[j] == "M") {
        m[RAM_coord[[j]]] <- df_values[j]
      }
    } # end loop with index j
    
    # Update sample covariance
    B <- solve(Ident - A)
    FB <- F_RAM %*% B
    E <- B %*% S %*% t(B)
    exp_cov <- F_RAM %*% E %*% t(F_RAM)
    exp_cov_inv <- solve(exp_cov)  
    
    # Update Jacobian matrix
    if (control$linear) { # Analytic Jacobian matrix
      
      for (j in seq_len(q)) {
        symm <- FB %*% A_deriv[[j]] %*% E %*% t(F_RAM)
        jac[p_star_seq, j] <- lavaan::lav_matrix_vech(symm + t(symm) + FB %*% S_deriv[[j]] %*% t(FB))
      }
      
      if (mean_structure) {
        for (j in seq_len(q)) {
          jac[(p_star+1):p_star_means, j] <- FB %*% A_deriv[[j]] %*% B %*% m +
            FB %*% m_deriv[[j]]
        }
      }
      
      
    } else { # Numeric Jacobian matrix
      x <- OpenMx::omxSetParameters(model = x, labels = df$labels,
                                    values = df_values[df_indices])
      x <- suppressMessages(OpenMx::mxRun(model = x, useOptimizer = FALSE))
      jac <- OpenMx::omxManifestModelByParameterJacobian(model = x)
    }
    
    if (mean_structure == FALSE) {jac <- jac[p_star_seq, , drop = FALSE]}
    
    
    V <- 0.5 * t(Dup) %*% kronecker(X = exp_cov_inv, Y = exp_cov_inv) %*% Dup
    if (mean_structure) {
      V_m_cov <- matrix(data = 0, nrow = p_star_means, ncol = p_star_means)
      V_m_cov[p_star_seq, p_star_seq] <- V
      V_m_cov[p_star_p_means_seq, p_star_p_means_seq] <- exp_cov_inv
      V <- V_m_cov
    }
    
    # Individual deviations from the sample moments
    vech_cov <- matrix(data = rep(x = lavaan::lav_matrix_vech(exp_cov),
                                  times = group_n),
                       byrow = TRUE, nrow = group_n, ncol = p_star)
    md[group_rows, 1:p_star] <- mc[group_rows, ] - vech_cov
    if (mean_structure) {
      means <- matrix(data = rep(x = FB %*% m, times = group_n), byrow = TRUE,
                      nrow = group_n, ncol = p)
      means_dev <- as.matrix(data_obs[group_rows, ]) - means
      md[group_rows, (p_star+1):p_star_means] <- means_dev
    }
    
    scores[group_rows, ]  <- md[group_rows, ] %*% V %*% jac
  }
  
  scores
  
}


identify_definition_variables <- function(x) {
  definition_variables <- c()
  for (i in 1:length(x@matrices)) {
    definition_variables <- c(definition_variables,
                              sapply(x@matrices[[i]]$labels,
                                     OpenMx::imxIsDefinitionVariable))
  }
  definition_variables <- names(which(definition_variables))
  list(labels = definition_variables, # OpenMx labels
       data = sub(".*\\.", "", definition_variables)) # column names
}

