#' Wrapper function for computing the maxLR corrected p value
#' from strucchange
#' @author Manuel Arnold, Andreas Brandmaier
#' @return Numeric. p value for maximally selected LR statistic
computePval_maxLR <- function(maxLR, q, covariate, from = 0.15, to = NULL,
                              nrep = 50000) {
  
  # Level of measurement
  if (!is.factor(covariate)) { # metric
    pval <- strucchange::supLM(from = from, to = to)$computePval(x = maxLR, nproc = q)
  } else {
    # sort observed levels of covariate
    covariate <- covariate[order(covariat)] 
    if (is.ordered(covariate)) { # ordinal
      pval <- strucchange::ordL2BB(freq = covariate, nproc = q,
                      nrep = nrep)$computePval(x = maxLR, nproc = q)
    } else { # categorical
      pval <- strucchange::catL2BB(factor(covariate))$computePval(x = maxLR, nproc = q)
    }
  }
  
  return(pval)
  
}
