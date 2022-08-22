vcov_semtree <- function(x, ...) {
  UseMethod("vcov_semtree")
}

vcov_semtree.default <- function(x, ...) {
  vcov(x, ...)
}

vcov_semtree.lavaan <- function(x, ...) {
  if (x@Model@eq.constraints) {
    K <- eval(parse(text = "lavaan:::lav_constraints_R2K(x@Model)"))
    res <- solve(t(K) %*% lavaan::lavInspect(x, what = "information.expected") %*% K *
                   nobs(x))
  } else {
    res <- x@vcov$vcov
  }
  res
}

vcov_semtree.ctsemFit <- function(x, ...) {

  ids <- which(colnames(x$mxobj$data$observed) %in%
                 grep(pattern = "^intervalID_T*",
                      x = colnames(x$mxobj$data$observed),
                      value = TRUE))

  dat <- x$mxobj$data$observed[, -ids]

  fit_untransformed <- ctsemOMX::ctFit(dat = dat,
                                       ctmodelobj = x$ctmodelobj,
                                       dataform = "wide",
                                       stationary = "all",
                                       fit = FALSE,
                                       omxStartValues = coef.ctsemFit(x),
                                       transformedParams = FALSE)

  fit_untransformed <- OpenMx::mxModel(
    model = fit_untransformed$mxobj,
    OpenMx::mxComputeSequence(steps = list(
      OpenMx::mxComputeNumericDeriv(checkGradient = FALSE,
                                    hessian = TRUE,
                                    analytic = FALSE)
    )))

  fit_untransformed <- OpenMx::mxRun(model = fit_untransformed, silent = TRUE)

  2 * solve(fit_untransformed$output$calculatedHessian)

}
