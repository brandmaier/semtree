getPredictorsOpenMx <- function(mxmodel, dataset, covariates)
{
  # specify covariates from model columns
  if (is.null(covariates)) {
    model.ids <- rep(NA_integer_, length(mxmodel@manifestVars))
    # find the ids in dataset
    for (i in seq_along(model.ids)) {
      cmp <- mxmodel@manifestVars[i] == names(dataset)
      if (all(!cmp)) {
        ui_stop("Error. Variable ", mxmodel@manifestVars[i], " missing in data set!")
      }
      model.ids[i] <- which(cmp)
    }
    all.ids <- 1:length(names(dataset))
    cvid <- all.ids[!all.ids %in% model.ids]
    if (length(cvid)==0) {
      ui_stop("Error. No predictors contained in dataset!")
    }
    covariate.ids <- simplify2array(as.vector(cvid, mode = "integer"))
  }
  # resort columns to organize covariates
  else {
    all.ids <- 1:length(names(dataset))
    covariate.ids <- match(covariates, names(dataset))
    if (any(is.na(covariate.ids))) {
      missing_covariates <- paste(covariates[is.na(covariate.ids)], collapse = ", ")
      ui_stop("Covariate(s) not found in dataset: ", missing_covariates)
    }

    modid <- all.ids[!all.ids %in% covariate.ids]
    if (length(modid)==0) {
      ui_stop("No covariates contained in dataset!")
    }

    model.ids <- simplify2array(as.vector(modid, mode = "integer"))
  }
  
  return(list(model.ids, covariate.ids))
}