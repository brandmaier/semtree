#' BORUTA algorithm for SEM trees
#' 
#' This is an experimental feature. Use cautiously.
#' 
#' @aliases boruta
#' @param model A template model specification from \code{\link{OpenMx}} using
#' the \code{\link{mxModel}} function (or a \code{\link[lavaan]{lavaan}} model
#' using the \code{\link[lavaan]{lavaan}} function with option fit=FALSE).
#' Model must be syntactically correct within the framework chosen, and
#' converge to a solution.
#' @param data Data.frame used in the model creation using
#' \code{\link{mxModel}} or \code{\link[lavaan]{lavaan}} are input here. Order
#' of modeled variables and predictors is not important when providing a
#' dataset to \code{semtree}.
#' @param control \code{\link{semtree}} model specifications from
#' \code{\link{semtree.control}} are input here. Any changes from the default
#' setting can be specified here.
#' @param percentile_threshold Numeric.
#' @param rounds Numeric. Number of rounds of the BORUTA algorithm.
#'
#' @export
#' 
boruta <- function(model,
                   data,
                   control = NULL,
                   predictors = NULL,
                   percentile_threshold = 1,
                   rounds = 1,
                   ...) {
  
  # initial checks
  stopifnot(percentile_threshold>=0)
  stopifnot(percentile_threshold<=1)
  stopifnot(is.numeric(rounds))
  stopifnot(rounds>0)
  
  preds_important <- c()
  preds_unimportant <- c()

  cur_round = 1
  temp_vims <- list()
   
  while(cur_round <= rounds) {
  vim_boruta <- .boruta(model=model,
          data=data,
          control=control,
          predictors=predictors,
          percentile_threshold = percentile_threshold,
          ...) 
  browser()
  # add predictors to list of unimportant variables
  preds_unimportant <- c(preds_unimportant, names(vim_boruta$filter)[!vim_boruta$filter])
  # remove them from the dataset
  data <- data[, -c(preds_unimportant)]
  temp_vims[[cur_round]] <-vim_boruta
  }
  
  result <- list(
    preds_unimportant,
    rounds = rounds
  )
  
  return(result)
}

.boruta <- function(model,
                   data,
                   control = NULL,
                   predictors = NULL,
                   percentile_threshold = 1,
                   num_shadows = 1,
                   ...) {
  
  # make sure that no column names start with "shadow_" prefix
  stopifnot(all(sapply(names(data), function(x) {!startsWith(x, "shadow_")})))

  # detect model (warning: duplicated code)
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    tmp <- getPredictorsOpenMx(mxmodel = model, dataset = data, covariates = predictors)

  } else if (inherits(model,"lavaan")){
    
    tmp <- getPredictorsLavaan(model, data, predictors)
  } else {
    ui_stop("Unknown model type selected. Use OpenMx or lavaanified lavaan models!")
  }
  model.ids <- tmp[[1]]
  covariate.ids <- tmp[[2]]

  # stage 1 - create shadow features

  shadow.ids <- (ncol(data) + 1):(ncol(data) + length(covariate.ids))

  for (cur_cov_id in covariate.ids) {
    for (rep_id in 1:num_shadows) {
    # pick column and shuffle
    temp_column <- data[, cur_cov_id]
    temp_column <- sample(temp_column, length(temp_column), replace = FALSE)
    # add to dataset as shadow feature
    temp_colname <- paste0("shadow_", names(data)[cur_cov_id], collapse = "")
    if (num_shadows>1) temp_colname <- paste0(temp_colname, rep_id, collapse = "")
    data[temp_colname] <- temp_column
    if (!is.null(predictors)) predictors <- c(predictors, temp_colname)
    }
  }

  # run the forest
  forest <- semforest(model, data, control, predictors, ...)

  # run variable importance
  vim <- varimp(forest)

  # get variable importance from shadow features
  shadow_names <- names(data)[shadow.ids]
  agvim <- aggregateVarimp(vim, aggregate = "mean")
  
  vals <- agvim[names(agvim) %in% shadow_names]
  #max_shadow_importance <- max(vals)
  max_shadow_importance <- quantile(vals, percentile_threshold)
  
  agvim_filtered <- agvim[!(names(agvim) %in% shadow_names)]

  df <- data.frame(importance = agvim_filtered, predictor = names(agvim_filtered))

  vim$filter <- agvim_filtered > max_shadow_importance
  vim$boruta <- TRUE
  vim$boruta_threshold <- max_shadow_importance
  vim$percentile_threshold <- percentile_threshold

  return(vim)
}
