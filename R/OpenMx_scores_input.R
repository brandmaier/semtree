OpenMx_scores_input <- function(x, control) {
  
  p <- length(x$manifestVars)
  mean_structure <- any(x$M$free)
  p_star <- p * (p + 1) / 2
  p_star_means <- p * (p + 3) / 2
  
  if (control$linear | imxHasDefinitionVariable(x)) {
    
    param_names <- names(x$output$estimate)
    q <- length(param_names)
    
    if (q==0) {
      ui_stop("Error in OpenMx_scores_input() function. There are no free parameters in the model estimates. Model not run or converged?")
    }
    
    q_seq <- seq_len(q)
    p_unf <- NROW(x$A$values)
  
    Zero <- matrix(0, nrow = p_unf, ncol = p_unf)
    A_deriv <- lapply(q_seq, function(x) {Zero})
    S_deriv <- A_deriv
    zero <- matrix(0, nrow = p_unf, ncol = 1)
    m_deriv <- lapply(q_seq, function(x) {zero})
    
    for (i in q_seq) {
      A_deriv[[i]][which(x$A$labels == param_names[i], arr.ind = TRUE)] <- 1
    }
    
    for (i in q_seq) {
      S_deriv[[i]][which(x$S$labels == param_names[i], arr.ind = TRUE)] <- 1
    }
    
    for (i in q_seq) {
      m_deriv[[i]][which(x$M$labels == param_names[i])] <- 1
    }
    
    scores_info <- list(p = p, mean_structure = mean_structure, p_star = p_star,
                        p_star_means = p_star_means, q = q, q_seq = q_seq,
                        p_unf = p_unf, A_deriv = A_deriv, S_deriv = S_deriv,
                        m_deriv = m_deriv)
    
  } else {
    
    scores_info <- list(p = p, mean_structure = mean_structure,
                        p_star = p_star, p_star_means = p_star_means)
    
  }
  
  return(scores_info)
  
}