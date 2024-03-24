getLikelihood <- function(model) {
  # this seems to be not generic enough
  #  return(OpenMx::mxEval(objective, model));

  msm <- getS3method("summary", "MxModel")
  # alternative:
  if (is.null(model)) {
    warning("NULL Model in getLikelihood()-call")
    return(NULL)
  }

  return(msm(model)$Minus2LogLikelihood)
}
