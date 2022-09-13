ctsemScores <- function(fit) {
  
  # Groups with identical time intervals ----
  intervalID <- fit$mxobj$data$observed[, grep(pattern = "^intervalID_T*",
                                               x = colnames(fit$mxobj$data$observed),
                                               value = TRUE)]
  N <- fit$mxobj$data$numObs
  
  groups <- rep(NA, times = N)
  group_counter <- 1
  
  for (i in seq_len(N)) {
    
    if (!is.na(groups[i])) next
    
    for (j in i:N) {
      
      if (all(intervalID[j, ] == intervalID[i, ])) {
        groups[j] <- group_counter
      }
      
    }
    
    group_counter <- group_counter + 1
    
  }
  
  
  
  # Calculate scores ----
  
  # Get model information ----
  
  # Number of processes
  n_processes <- fit$ctmodelobj$n.latent
  n_parameters <- length(fit$mxobj$output$estimate)
  names_parameters <- names(fit$mxobj$output$estimate)
  names_manifestVars <- paste0(fit$ctmodelobj$manifestNames, "_T", 
                               rep(0:(fit$ctmodelobj$Tpoints - 1),
                                   each = fit$ctmodelobj$n.manifest))
  n_manifestVars <- length(names_manifestVars)
  # Determine free parameters
  free_mvar <- ifelse(test = any(fit$mxobj$MANIFESTVARbase$free),
                      yes = TRUE,
                      no = FALSE)
  
  free_cint <- ifelse(test = any(fit$mxobj$CINT$free),
                      yes = TRUE,
                      no = FALSE)
  
  free_trait <- ifelse(test = any(fit$mxobj$TRAITVARbase$free),
                       yes = TRUE,
                       no = FALSE)
  
  # Column-wise parameter names
  names_drift_parameters <- c(fit$mxobj$DRIFT$labels)
  if (free_mvar) {
    names_mvar_parameters <- as.character(stats::na.omit(c(fit$mxobj$MANIFESTVARbase$labels)))
  }
  names_diffusion_parameters <- as.character(stats::na.omit(c(fit$mxobj$DIFFUSIONbase$labels)))
  if (free_cint) {
    names_cint_parameters <- as.character(stats::na.omit(c(fit$mxobj$CINT$labels)))
  }
  if (free_trait) {
    names_trait_parameters <- as.character(stats::na.omit(c(fit$mxobj$TRAITVARbase$labels)))
  }
  # Number of unique time intervals
  n_unique_intervals <- max(intervalID)
  n_timepoints <- fit$ctmodelobj$Tpoints
  # Observed data
  data_obs <- fit$mxobj$data$observed[, names_manifestVars]
  ASm_dimensions <- nrow(fit$mxobj$A$values)
  
  # Constant matrices
  ## 0-1 matrices
  I_n_processes <- diag(1, nrow = n_processes)
  I_n_processes2 <- diag(1, nrow = n_processes^2)
  I_RAM <- diag(1, nrow = nrow(fit$mxobj$A$values))
  
  # Matrices consisting of parameters
  if (free_cint) {drift_inv <- fit$mxobj$invDRIFT$result}
  drift_hash <- fit$mxobj$DRIFTHATCH$result
  drift_hash_inv <- solve(drift_hash)
  Q_row <- matrix(fit$mxobj$DIFFUSION$result, nrow = n_processes^2, ncol = 1, # check
                  byrow = TRUE)
  
  scores <- matrix(NA, nrow = N, ncol = n_parameters,
                   dimnames = list(NULL, names_parameters))
  
  
  # Prepare lists with derivatives of RAM matrices ----
  A_deriv <- replicate(n = n_parameters,
                       expr = matrix(0, nrow = ASm_dimensions,
                                     ncol = ASm_dimensions),
                       simplify = FALSE)
  
  names(A_deriv) <- names_parameters
  
  S_deriv <- A_deriv
  
  if (free_cint) {
    m_deriv <- replicate(n = n_parameters,
                         expr = matrix(0, nrow = ASm_dimensions,
                                       ncol = 1),
                         simplify = FALSE)
    names(m_deriv) <- names_parameters
  }
  
  # Prepare lists with derivatives of Sigma ----
  Sigma_deriv <- replicate(n = n_parameters,
                           expr = matrix(0, nrow = n_manifestVars,
                                         ncol = n_manifestVars),
                           simplify = FALSE)
  names(Sigma_deriv) <- names_parameters
  
  if (free_cint) {
    mu_deriv <- replicate(n = n_parameters,
                          expr = matrix(0, nrow = n_manifestVars,
                                        ncol = 1),
                          simplify = FALSE)
    names(mu_deriv) <- names_parameters
  }
  
  # Constant derivatives of A and S matrices ----
  
  # mvar
  if (free_mvar) {
    names_MANIFESTVAR <- rep(NA, n_processes)
    for (i in 1:n_processes) {
      names_MANIFESTVAR[i] <- paste0("MANIFESTVAR[", i, ",", i, "]")
    }
    
    for (i in 1:n_processes) {
      S_deriv[[names_mvar_parameters[i]]] <- ifelse(test = fit$mxobj$S$labels == names_MANIFESTVAR[i],
                                                    yes = 1,
                                                    no = 0)
      S_deriv[[names_mvar_parameters[i]]][is.na(S_deriv[[names_mvar_parameters[i]]])] <- 0
    }
  }
  
  
  # traitvar
  if (free_trait) {
    trait_labels <- fit$mxobj$TRAITVARbase$labels
    trait_labels[upper.tri(trait_labels)] <- t(trait_labels)[upper.tri(trait_labels)]
    indices_trait_in_S <- (n_processes*n_timepoints+1):(n_processes*n_timepoints+n_processes)
    
    for (i in seq_along(names_trait_parameters)) {
      S_deriv[[names_trait_parameters[i]]][indices_trait_in_S, indices_trait_in_S] <-
        ifelse(test = trait_labels == names_trait_parameters[i],
               yes = 1,
               no = 0)
    }
  }
  
  
  
  # Derivatives of time-invariant matrices ----
  derivatives_drift <- lapply(c(fit$mxobj$DRIFT$labels), FUN = function(x) {
    ifelse(test = fit$mxobj$DRIFT$labels == x, yes = 1, no = 0)})
  
  epsilon <- lapply(c(fit$mxobj$DRIFT$labels), FUN = function(x) {
    ifelse(test = fit$mxobj$DRIFT$labels == x, yes = 0.0001, no = 0)})
  
  derivatives_drift_hash <- lapply(derivatives_drift, FUN = function(x) {
    kronecker(X = x, Y = I_n_processes) + kronecker(X = I_n_processes, Y = x)})
  
  derivatives_drift_hash_inv <- lapply(derivatives_drift_hash, function(x) {
    -drift_hash_inv %*% x %*% drift_hash_inv})
  names(derivatives_drift_hash_inv) <- names_drift_parameters
  
  derivatives_theta_0_drift <- lapply(derivatives_drift_hash_inv,
                                      FUN = function(x) {
                                        matrix(-x %*% Q_row, nrow = n_processes, ncol = n_processes)})
  
  names(derivatives_theta_0_drift) <- names_drift_parameters
  
  diffusion_labels_row <- fit$mxobj$DIFFUSIONbase$labels
  diffusion_labels_row[upper.tri(diffusion_labels_row)] <- t(diffusion_labels_row)[upper.tri(diffusion_labels_row)]
  diffusion_labels_row <- matrix(diffusion_labels_row, nrow = n_processes^2,
                                 ncol = 1, byrow = TRUE)
  
  derivatives_Q_row <- lapply(unique(c(diffusion_labels_row)), FUN = function(x) {
    ifelse(test = diffusion_labels_row == x, yes = 1, no = 0)})
  
  derivatives_theta_0_diffusion <- lapply(derivatives_Q_row, FUN = function(x) {
    -solve(fit$mxobj$DRIFTHATCH$result) %*% x})
  
  names(derivatives_theta_0_diffusion) <- names_diffusion_parameters
  
  if (free_cint) {
    derivatives_drift_inv <- lapply(derivatives_drift, FUN = function(x) {
      -drift_inv %*% x %*% drift_inv})
    
    derivatives_cint <- lapply(c(fit$mxobj$CINT$labels), FUN = function(x) {
      ifelse(test = fit$mxobj$CINT$labels == x, yes = 1, no = 0)})
    
    derivatives_tau_0_drift <- lapply(derivatives_drift_inv, FUN = function(x) {
      -x %*% fit$mxobj$CINT$values})
    names(derivatives_tau_0_drift) <- names_drift_parameters
    
    derivatives_tau_0_cint <- lapply(derivatives_cint, FUN = function(x) {
      -drift_inv %*% x})
    names(derivatives_tau_0_cint) <- names_cint_parameters
  }
  
  
  
  # Derivatives of time-variant matrices ----
  
  # [[parameter]][[time_interval]]
  derivatives_expm_drift <- replicate(
    replicate(
      matrix(NA, nrow = n_processes, ncol = n_processes),
      n = n_unique_intervals, simplify = FALSE),
    n = n_processes^2, simplify = FALSE)
  
  
  for (i in seq_len(n_unique_intervals)) {
    select_time_interval <- fit$mxobj[[paste0("discreteDRIFT_i", i)]]$formula[[2]][[3]]
    for(j in seq_len(n_processes^2)) {
      derivatives_expm_drift[[j]][[i]] <-
        (expm::expm(select_time_interval * (fit$mxobj$DRIFT$values + epsilon[[j]])) -
           expm::expm(select_time_interval * (fit$mxobj$DRIFT$values - epsilon[[j]]))) /
        (2 * 0.0001)
    }
  } 
  
  names(derivatives_expm_drift) <- names_drift_parameters
  
  
  derivatives_expm_drift_hash <- replicate(
    replicate(
      matrix(NA, nrow = n_processes^2, ncol = n_processes^2),
      n = n_unique_intervals, simplify = FALSE),
    n = n_processes^2, simplify = FALSE)
  
  for (i in seq_len(n_unique_intervals)) {
    select_time_interval <- fit$mxobj[[paste0("discreteDRIFT_i", i)]]$formula[[2]][[3]]
    for(j in seq_len(n_processes^2)) {
      kronecker_product1 <- 
        kronecker(X = fit$mxobj$DRIFT$values + epsilon[[j]], Y = I_n_processes) +
        kronecker(X = I_n_processes, Y = fit$mxobj$DRIFT$values + epsilon[[j]])
      kronecker_product2 <- 
        kronecker(X = fit$mxobj$DRIFT$values - epsilon[[j]], Y = I_n_processes) +
        kronecker(X = I_n_processes, Y = fit$mxobj$DRIFT$values - epsilon[[j]])
      derivatives_expm_drift_hash[[j]][[i]] <-
        (expm::expm(select_time_interval * kronecker_product1) - 
           expm::expm(select_time_interval * kronecker_product2)) /
        (2 * 0.0001)
    }
  }
  
  names(derivatives_expm_drift_hash) <- names_drift_parameters
  
  
  # [[parameter]][[time_interval]]
  derivatives_theta_drift <- replicate(
    replicate(
      matrix(NA, nrow = n_processes, ncol = n_processes),
      n = n_unique_intervals, simplify = FALSE),
    n = n_processes^2, simplify = FALSE)
  
  for (i in seq_len(n_unique_intervals)) {
    delta_t <- fit$mxobj[[paste0("discreteDRIFT_i", i)]]$formula[[2]][[3]]
    expm_drift_hash_delta_t <- expm::expm(fit$mxobj$DRIFTHATCH$result * delta_t)
    expm_drift_hash_delta_t_I_n_processes2 <- expm_drift_hash_delta_t - I_n_processes2
    drift_hash_inv_delta_t_expm <- drift_hash_inv %*% (delta_t * expm_drift_hash_delta_t)
    for(j in seq_len(n_processes^2)) {
      term1 <- derivatives_drift_hash_inv[[j]] %*% expm_drift_hash_delta_t_I_n_processes2
      term2 <- drift_hash_inv %*% derivatives_expm_drift_hash[[j]][[i]]
      derivatives_theta_drift[[j]][[i]] <-
        matrix((term1 + term2) %*% Q_row, nrow = n_processes, ncol = n_processes,
               byrow = TRUE)
    }
  }
  
  names(derivatives_theta_drift) <- names_drift_parameters
  
  
  # [[parameter]][[time_interval]]
  derivatives_theta_Q <- replicate(
    replicate(
      matrix(NA, nrow = n_processes, ncol = n_processes),
      n = n_unique_intervals, simplify = FALSE),
    n = n_processes*(n_processes+1)/2, simplify = FALSE)
  
  for (i in seq_len(n_unique_intervals)) {
    delta_t <- fit$mxobj[[paste0("discreteDRIFT_i", i)]]$formula[[2]][[3]]
    expm_drift_hash_delta_t <- expm::expm(fit$mxobj$DRIFTHATCH$result * delta_t)
    expm_drift_hash_delta_t_I_n_processes2 <- expm_drift_hash_delta_t - I_n_processes2
    for(j in seq_len(n_processes*(n_processes+1)/2)) {
      derivatives_theta_Q[[j]][[i]] <- matrix(drift_hash_inv %*%
                                                expm_drift_hash_delta_t_I_n_processes2 %*%
                                                derivatives_Q_row[[j]], nrow = n_processes, ncol = n_processes, byrow = TRUE)
    }
  } 
  
  names(derivatives_theta_Q) <- names_diffusion_parameters
  
  if (free_cint) {
    
    # [[parameter]][[time_interval]]
    derivatives_tau_drift <- replicate(
      replicate(
        matrix(NA, nrow = n_processes, ncol = 1),
        n = n_unique_intervals, simplify = FALSE),
      n = n_processes^2, simplify = FALSE)
    
    for (i in seq_len(n_unique_intervals)) {
      delta_t <- fit$mxobj[[paste0("discreteDRIFT_i", i)]]$formula[[2]][[3]]
      expm_drift_delta_t <- expm::expm(fit$mxobj$DRIFT$values * delta_t)
      expm_drift_delta_t_I_n_processes <- expm_drift_delta_t - I_n_processes
      for(j in seq_len(n_processes^2)) {
        res <- derivatives_drift_inv[[j]] %*% 
          expm_drift_delta_t_I_n_processes +
          drift_inv %*% derivatives_expm_drift[[j]][[i]]
        derivatives_tau_drift[[j]][[i]] <- res %*% fit$mxobj$CINT$values
      }
    }
    
    names(derivatives_tau_drift) <- names_drift_parameters
    
    
    # [[parameter]][[time_interval]]
    derivatives_tau_cint <- replicate(
      replicate(
        matrix(NA, nrow = n_processes, ncol = 1),
        n = n_unique_intervals, simplify = FALSE),
      n = n_processes, simplify = FALSE)
    
    for (i in seq_len(n_unique_intervals)) {
      delta_t <- fit$mxobj[[paste0("discreteDRIFT_i", i)]]$formula[[2]][[3]]
      expm_drift_delta_t <- expm::expm(fit$mxobj$DRIFT$values * delta_t)
      expm_drift_delta_t_I_n_processes <- expm_drift_delta_t - I_n_processes
      for(j in seq_len(n_processes)) {
        derivatives_tau_cint[[j]][[i]] <-
          drift_inv %*% expm_drift_delta_t_I_n_processes %*% derivatives_cint[[j]]
      }
    }
    
    names(derivatives_tau_cint) <- names_cint_parameters
    
  }
  
  # Indices ----
  
  indices_drift <- lapply(1:(n_timepoints - 1), function(x)
    list(rows = (x*n_processes+1):((x+1)*n_processes),
         cols = ((x-1)*n_processes+1):(x*n_processes)))
  
  indices_theta_0 <- 1:n_processes
  
  indices_diffusion <- lapply(2:n_timepoints, function(x)
    ((x-1)*n_processes+1):(x*n_processes))
  
  if (free_cint) {
    indices_tau_0 <- indices_theta_0
    indices_tau <- indices_diffusion
  }
  
  if (free_trait) {
    indices_trait <- lapply(2:n_timepoints, function(x)
      ((x-1)*n_processes+1):(x*n_processes))
  }
  
  
  
  for (g in unique(groups)) {
    
    # Select data
    
    select_data <- data_obs[groups == g, , drop = FALSE]
    n_select_data <- nrow(select_data)
    select_ti <- intervalID[groups == g, , drop = FALSE][1, ]  
    
    
    # RAM matrices ----
    F_mat <- fit$mxobj$F$values
    
    S_mat <- fit$mxobj$S$values
    
    A_mat <- fit$mxobj$A$values
    
    if (free_cint) {
      m_mat <- t(fit$mxobj$M$values)
    }
    
    # Update diffusion matrices in S matrix
    for (i in seq_len(n_timepoints - 1)) {
      S_mat[indices_diffusion[[i]], indices_diffusion[[i]]] <-
        fit$mxobj[[paste0("discreteDIFFUSION_i", select_ti[i])]]$result
    }
    
    # Update drift matrices in A matrix
    for (i in seq_len(n_timepoints - 1)) {
      A_mat[indices_drift[[i]]$rows, indices_drift[[i]]$cols] <-
        fit$mxobj[[paste0("discreteDRIFT_i", select_ti[i])]]$result
    }
    
    # Update cint in m matrix
    if (free_cint) {
      for (i in seq_len(n_timepoints - 1)) {
        m_mat[indices_tau[[i]], 1] <-
          fit$mxobj[[paste0("discreteCINT_i", select_ti[i])]]$result
      } 
    }
    
    # Update trait effects in A matrix
    if (free_trait) {
      trait_cols <- (n_processes * n_timepoints + 1):(n_processes * (1 + n_timepoints))
      for (i in seq_len(n_timepoints - 1)) {
        A_mat[indices_trait[[i]], trait_cols] <-
          I_n_processes - fit$mxobj[[paste0("discreteDRIFT_i", select_ti[i])]]$result
      }
    }
    
    
    
    # Drift parameters ----
    
    for (select_par in names_drift_parameters) {
      
      # A matrix
      # Derivatives of expm(drift*t)
      # and derivatives of trait effects
      for (i in seq_len(n_timepoints - 1)) {
        A_deriv[[select_par]][indices_drift[[i]]$rows, indices_drift[[i]]$cols] <-
          derivatives_expm_drift[[select_par]][[select_ti[i]]]
        if (free_trait) {
          A_deriv[[select_par]][indices_trait[[i]], trait_cols] <-
            -derivatives_expm_drift[[select_par]][[select_ti[i]]]
        }
      }
      
      # S matrix
      # Derivatives of Theta_0
      # Derivatives of Theta
      S_deriv[[select_par]][indices_theta_0, indices_theta_0] <- 
        derivatives_theta_0_drift[[select_par]]
      for (i in seq_len(n_timepoints - 1)) {
        S_deriv[[select_par]][indices_diffusion[[i]], indices_diffusion[[i]]] <-
          derivatives_theta_drift[[select_par]][[select_ti[i]]]
      }
      
      # m Matrix
      # Derivatives of tau_0
      # Derivatives of tau
      if (free_cint) {
        m_deriv[[select_par]][indices_tau_0, 1] <- 
          derivatives_tau_0_drift[[select_par]]
        for (i in seq_len(n_timepoints - 1)) {
          m_deriv[[select_par]][indices_tau[[i]], 1] <-
            derivatives_tau_drift[[select_par]][[select_ti[i]]]
        }
      }
      
    }
    
    
    # Diffusion parameters ----
    
    for (select_par in names_diffusion_parameters) {
      
      # S matrix
      # Derivatives of Theta_0
      # Derivatives of Theta
      S_deriv[[select_par]][indices_theta_0, indices_theta_0] <- 
        derivatives_theta_0_diffusion[[select_par]]
      for (i in seq_len(n_timepoints - 1)) {
        S_deriv[[select_par]][indices_diffusion[[i]], indices_diffusion[[i]]] <-
          derivatives_theta_Q[[select_par]][[select_ti[i]]]
      }
      
    }
    
    
    # Cint parameters ----
    
    if (free_cint) {
      for (select_par in names_cint_parameters) {
        
        # m matrix
        # Derivatives of tau_0
        # Derivatives of tau
        m_deriv[[select_par]][indices_tau_0, 1] <- 
          derivatives_tau_0_cint[[select_par]]
        for (i in seq_len(n_timepoints - 1)) {
          m_deriv[[select_par]][indices_tau[[i]], 1] <-
            derivatives_tau_cint[[select_par]][[select_ti[i]]]
        }
      }
    }
    
    
    
    # Prepare RAM matrices ----
    
    I_A_inv <- solve(I_RAM - A_mat)
    F_I_A_inv <- F_mat %*% I_A_inv
    F_I_A_inv_S <- F_I_A_inv %*% S_mat
    F_I_A_inv_S_I_A_inv <- F_I_A_inv_S %*% t(I_A_inv)
    Sigma <- F_I_A_inv_S_I_A_inv %*% t(F_mat)
    Sigma_inv <- solve(Sigma)
    if (free_cint) {
      mu <- F_I_A_inv %*% m_mat
      mu_matrix <- matrix(mu, nrow = n_select_data, ncol = n_manifestVars,
                          byrow = TRUE)
    }
    
    
    
    # Compute derivative of Sigma and mu ----
    for (select_par in names_drift_parameters) {
      Sigma_deriv_P1 <- F_I_A_inv %*% S_deriv[[select_par]] %*% t(F_I_A_inv)
      Sigma_deriv_P2 <- F_I_A_inv %*% A_deriv[[select_par]] %*%
        t(F_I_A_inv_S_I_A_inv)
      Sigma_deriv[[select_par]] <- Sigma_deriv_P1 + Sigma_deriv_P2 + t(Sigma_deriv_P2)
      
      if (free_cint) {
        mu_deriv[[select_par]] <- F_I_A_inv %*% A_deriv[[select_par]] %*% I_A_inv %*% m_mat +
          F_I_A_inv %*% m_deriv[[select_par]]
      }
    }
    
    if (free_mvar) {
      for (select_par in names_mvar_parameters) {
        Sigma_deriv[[select_par]] <- F_I_A_inv %*% S_deriv[[select_par]] %*% t(F_I_A_inv)
      }
    }
    
    for (select_par in names_diffusion_parameters) {
      Sigma_deriv[[select_par]] <- F_I_A_inv %*% S_deriv[[select_par]] %*% t(F_I_A_inv)
    }
    
    if (free_cint) {
      for (select_par in names_cint_parameters) {
        mu_deriv[[select_par]] <- F_I_A_inv %*% m_deriv[[select_par]]
      }
    }
    
    if (free_trait) {
      for (select_par in names_trait_parameters) {
        Sigma_deriv[[select_par]] <- F_I_A_inv %*% S_deriv[[select_par]] %*% t(F_I_A_inv)
      }
    }
    
    
    # Compute scores ----
    
    for (select_par in names_parameters) {
      
      if (free_cint) {
        
        select_data_centered <- select_data - mu_matrix
        term1 <- -2 * select_data_centered %*% Sigma_inv %*% mu_deriv[[select_par]]
        term2 <- -.rowSums(select_data_centered *
                             (select_data_centered %*% Sigma_inv %*%
                                Sigma_deriv[[select_par]] %*% Sigma_inv),
                           m = n_select_data, n = n_manifestVars)
        Trace <- sum(diag(Sigma_inv %*% Sigma_deriv[[select_par]]))
        scores[groups == g, select_par] <- term1 + term2 + Trace
      } else {
        term2 <- -.rowSums(select_data * (select_data %*% Sigma_inv %*%
                                            Sigma_deriv[[select_par]] %*%
                                            Sigma_inv),
                           m = n_select_data, n = n_manifestVars)
        Trace <- sum(diag(Sigma_inv %*% Sigma_deriv[[select_par]]))
        scores[groups == g, select_par] <- term2 + Trace
      }
    }
  }
  
  -1/2 * scores
}
