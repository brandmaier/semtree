sctest_semtree <- function (x, order.by = NULL, functional = maxBB, vcov = NULL, 
                            scores = estfun, decorrelate = TRUE, sandwich = TRUE, parm = NULL, 
                            plot = FALSE, from = 0.1, to = NULL, nobs = NULL, nrep = 50000, 
                            width = 0.15, xlab = NULL, functional_string, scaled_split = TRUE,
                            ...) 
{
 
  if (is.character(functional)) {
    functional <- tolower(functional)
    functional <- switch(functional, dmax = "dm", maxlm = "suplm", 
                         mosum = "maxmosum", functional)
    functional_string <- functional
    if (is.null(order.by) & functional %in% c("lmuo", 
                                              "wdmo", "maxlmo")) {
      stop("'order.by' must provide a grouping of the observations")
    }
    functional <- switch(functional, dm = maxBB, cvm = meanL2BB, 
                         suplm = supLM(from = from, to = to), range = rangeBB, 
                         lmuo = catL2BB(factor(order.by)), wdmo = ordwmax(factor(order.by)), 
                         maxlmo = ordL2BB(factor(order.by), nproc = NCOL(scus$process), 
                                          nobs = nobs, nrep = nrep), maxmosum = maxMOSUM(width = width), 
                         stop("Unknown efp functional."))
  }
  if (plot) 
    plot(scus, functional = functional, xlab = xlab, ...)
  rval <- c(sctest(scus,
                   functional = functional,
                   functional_string = functional_string),
            sctest_semtree_info(CSP = as.matrix(scus$process),
                                covariate = order.by[order(order.by)],
                                functional_string = functional_string,
                                scaled_split = scaled_split,
                                from = from,
                                to = to))
  rval$data.name <- nam
  return(rval)
}