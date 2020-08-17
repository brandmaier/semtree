#' Wrapper function for computing the maxLR corrected p value
#' from strucchange
# maxLR: maximum of the LR test statistics
# q: number of free SEM parameters / degrees of freedom
# covariate: covariate under evaluation. This is important to get the level of
#            measurement from the covariate and the bin size for ordinal and
#            categorical covariates.
# from, to: numeric from interval (0, 1) specifying start and end of trimmed
#           sample period. By default, to is 1 - from, i.e., with the default 
#           from = 0.15 the first and last 15 percent of observations are
#           trimmed. This only needed for continuous covariates.
# nrep: numeric. Number of replications used for simulating from the asymptotic 
#       distribution (passed to efpFunctional). Only needed for ordinal
#       covariates.
#' @author Manuel Arnold
#' @return Numeric. p value for maximally selected LR statistic
  computePval_maxLR <- function(maxLR, q, covariate, from = 0.15, to = NULL,
                                nrep = 50000) {
    
    # Level of measurement
    if (!is.factor(covariate)) { # metric
      pval <- supLM(from = from, to = to)$computePval(x = maxLR, nproc = q)
    } else {
      covariate <- covariate[order(covariate)] # sort covariate
      if (is.ordered(covariate)) { # ordinal
        pval <- ordL2BB(freq = covariate, nproc = q,
                        nrep = nrep)$computePval(x = maxLR, nproc = q)
      } else { # categorical
        pval <- catL2BB(covariate)$computePval(x = maxLR, nproc = q)
      }
    }
    
    pval
    
  }
  
