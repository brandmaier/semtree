sctest_info <- function(CSP, covariate, test, scaled_split, from, to) {
  # Continuous covariate
  if (test == "dm") {
    CSP <- CSP[-1, , drop = FALSE]
    abs_CSP <- abs(x = CSP)
    contrib <- apply(X = abs_CSP, MARGIN = 2, FUN = max)
    if (scaled_split) {
      cutpoint <- scaled_cutpoint(CSP = CSP, covariate = covariate, from = from,
                                  to = to)
    } else {
      max.cov <- covariate[which(x = abs_CSP == max(contrib), arr.ind = TRUE)[1, 1]]
      cutpoint <- (max.cov + covariate[which(covariate > max.cov)[1]]) / 2
    }
    left_n <- sum(covariate < cutpoint)
    right_n <- sum(covariate > cutpoint)
  }
  
  if (test == "cvm") {
    CSP <- CSP[-1, , drop = FALSE]
    CSP2 <- CSP^2
    contrib <- apply(X = CSP, MARGIN = 2, FUN = mean)
    if (scaled_split) {
      cutpoint <- scaled_cutpoint(CSP = CSP, covariate = covariate, from = from,
                                  to = to)
      if (is.na(cutpoint)) {scaled_split <- FALSE}
    }
    if (isFALSE(scaled_split)) {
      rows <- apply(X = CSP2, MARGIN = 1, FUN = sum)
      max.cov <- covariate[which.max(rows)]
      cutpoint <- (max.cov + covariate[which(covariate > max.cov)[1]]) / 2
      if (covariate[which.max(rows)] == max(covariate)) {
        unique.cov <- unique(covariate)
        cutpoint <- (unique.cov[length(unique.cov) - 1] + unique.cov[length(unique.cov)]) / 2
      }
    }
    left_n <- sum(covariate < cutpoint)
    right_n <- sum(covariate > cutpoint)
  }
  
  if (test == "suplm") {
    CSP <- CSP[-1, , drop = FALSE]
    CSP2 <- CSP^2
    rows <- apply(X = CSP2, MARGIN = 1, FUN = sum)
    n <- length(rows)
    n1 <- floor(from * n)
    n2 <- floor(to * n)
    tt <- seq_along(rows)/n
    CSP2 <- CSP2[n1:n2, , drop = FALSE]
    rows <- rows[n1:n2]
    tt <- tt[n1:n2]
    scaling_factor <- tt * (1 - tt)
    CSP2 <- CSP2 / scaling_factor 
    rows <- rows / scaling_factor
    contrib <- apply(X = CSP2, MARGIN = 2, FUN = max)
    max.cov <- covariate[which.max(rows) + n1 - 1]
    cutpoint <- (max.cov + covariate[which(covariate > max.cov)[1]]) / 2
    left_n <- sum(covariate < cutpoint)
    right_n <- sum(covariate > cutpoint)
  }
  
  # Ordinal covariate
  if (test == "wdmo") {
    covariate <- droplevels(covariate)
    CSP <- CSP[-1, , drop = FALSE]
    freq <- prop.table(table(covariate))
    freq <- freq / sum(freq)
    ncat <- length(freq)
    tcat <- cumsum(freq[-ncat])
    n <- NROW(CSP)
    tt <- 1:n / n
    ix <- round(tcat * n)
    CSP <- CSP[ix, , drop = FALSE]
    tt <- tt[ix]
    CSP <- abs(CSP)
    CSP <- CSP / sqrt(tt * (1 - tt))
    contrib <- apply(X = CSP, MARGIN = 2, FUN = max)
    max.cov <- which(CSP == max(contrib), arr.ind = TRUE)[1, 1]
    cutpoint <- mean(as.numeric(levels(covariate)[max.cov:(max.cov + 1)]))
    left_n <- sum(as.numeric(levels(covariate))[covariate] < cutpoint)
    right_n <- sum(as.numeric(levels(covariate))[covariate] > cutpoint)
  }
  
  if (test == "maxlmo") {
    covariate <- droplevels(covariate)
    CSP <- CSP[-1, , drop = FALSE]
    freq <- prop.table(table(covariate))
    freq <- freq / sum(freq)
    ncat <- length(freq)
    tcat <- cumsum(freq[-ncat])
    n <- NROW(CSP)
    tt <- 1:n / n
    ix <- round(tcat * n)
    CSP <- CSP[ix, , drop = FALSE]
    tt <- tt[ix]
    CSP2 <- CSP^2
    CSP2 <- CSP2 / (tt * (1 - tt))
    contrib <- apply(X = CSP2, MARGIN = 2, FUN = sum)
    max.cov <- which(CSP2 == max(CSP2), arr.ind = TRUE)[1, 1]
    cutpoint <- mean(as.numeric(levels(covariate)[max.cov:(max.cov + 1)]))
    left_n <- sum(as.numeric(levels(covariate))[covariate] < cutpoint)
    right_n <- sum(as.numeric(levels(covariate))[covariate] > cutpoint)
  }
  
  # Nominal covariates
  if (test == "lmuo") {
    covariate <- droplevels(covariate)
    CSP <- CSP[-1, , drop = FALSE]
    freq <- prop.table(table(covariate))
    freq <- freq / sum(freq)
    ncat <- length(freq)
    n <- NROW(CSP)
    d <- diff(rbind(0, CSP[round(cumsum(freq) * n), ]))
    contrib <- apply(X = d, MARGIN = 2, FUN = function(x) {sum(x^2 / freq)})
    if (nlevels(covariate) <= 2) {
      cutpoint <- 1
      left_n <- length(covariate[covariate == levels(covariate)[1]])
      right_n <- length(covariate[covariate == levels(covariate)[2]])
    } else {
      cutpoint <- left_n <- right_n <-  NULL 
    }
  }
  
  # Prepare output
  rval <- list(par.contrib = contrib, cutpoint = cutpoint, left_n = left_n,
               right_n = right_n)
  return(rval)
}
