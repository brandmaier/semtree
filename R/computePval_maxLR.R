#' Wrapper function for computing the maxLR corrected p value
#' from strucchange
#' 
#' @param maxLR maximum of the LR test statistics
#' @param q number of free SEM parameters / degrees of freedom
#' @param covariate covariate under evaluation. This is important to get the level of
#'            measurement from the covariate and the bin size for ordinal and
#'            categorical covariates.
#' @param from numeric from interval (0, 1) specifying start of trimmed
#'           sample period.  With the default 
#'           from = 0.15 the first and last 15 percent of observations are
#'           trimmed. This is only needed for continuous covariates.
#' @param to numeric from interval (0, 1) specifying end of trimmed
#'           sample period. By default, to is 1.
#' @param nrep numeric. Number of replications used for simulating from the asymptotic 
#'       distribution (passed to efpFunctional). Only needed for ordinal
#'       covariates.
#'
#
#' @author Manuel Arnold
#' @return Numeric. p value for maximally selected LR statistic

  computePval_maxLR <- function(maxLR, q, covariate, from, to, nrep) {
    
    # Level of measurement
    if (!is.factor(covariate)) { # metric
      pval <- strucchange::supLM(from = from, to = to)$computePval(x = maxLR, nproc = q)
    } else {
      covariate <- sort(covariate) # sort covariate
      covariate <- droplevels(covariate)
      if (is.ordered(covariate)) { # ordinal
        pval <- strucchange::ordL2BB(freq = covariate, nproc = q,
                        nrep = nrep)$computePval(x = maxLR, nproc = q)
      } else { # categorical
        pval <- strucchange::catL2BB(covariate)$computePval(x = maxLR, nproc = q)
      }
    }
    
    pval
    
  }
  
