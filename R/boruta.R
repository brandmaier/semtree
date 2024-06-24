
#' Run the Boruta algorithm on a sem tree
#' 
#' Grows a series of SEM Forests following the boruta algorithm to determine
#'    feature importance as moderators of the underlying model.
#' 
#' 
#' @aliases boruta plot.boruta print.boruta
#' @param model A template SEM. Same as in \code{semtree}.
#' @param data A dataframe to boruta on. Same as in \code{semtree}.
#' @param control A semforest control object to set forest parameters.
#' @param predictors An optional list of covariates. See semtree code example.
#' @param constraints An optional list of covariates. See semtree code example.
#' @param \dots Optional parameters.
#' @return A boruta object.
#' @author Priyanka Paul, Timothy R. Brick, Andreas Brandmaier
#' @seealso \code{\link{semtree}} \code{\link{semforest}}
#' 
#' @keywords tree models multivariate
#' @export
boruta <- function(model,
                   data,
                   control = NULL,
                   predictors = NULL,
                   ...) {
  
 
  # TODO: make sure that no column names start with "shadow_" prefix
  
  # detect model (warning: duplicated code)
  if (inherits(model,"MxModel") || inherits(model,"MxRAMModel")) {
    tmp <- getPredictorsOpenMx(mxmodel=model, dataset=data, covariates=predictors)
    model.ids <- tmp[[1]]
    covariate.ids <- tmp[[2]]
#  } else if (inherits(model,"lavaan")){

 # } else if ((inherits(model,"ctsemFit")) || (inherits(model,"ctsemInit"))) {
#
  } else {
    ui_stop("Unknown model type selected. Use OpenMx or lavaanified lavaan models!");
  }
  
  # stage 1 - create shadow features

  shadow.ids <- (ncol(data)+1):(ncol(data)+length(covariate.ids))
  
  for (cur_cov_id in covariate.ids) {
    # pick column and shuffle
    temp_column <- data[, cur_cov_id]
    temp_column <- sample(temp_column, length(temp_column), replace=FALSE)
    # add to dataset as shadow feature
    temp_colname <- paste0("shadow_", names(data)[cur_cov_id],collapse="")
    data[temp_colname] <- temp_column
    if (!is.null(predictors)) predictors <- c(predictors, temp_colname)
  }
  
  # run the forest
  forest <- semforest(model, data, control, predictors, ...)
  
  # run variable importance
  vim <- varimp(forest)
  
  # get variable importance from shadow features
  shadow_names <- names(data)[shadow.ids]
  agvim <- aggregateVarimp(vim, aggregate="mean")
  max_shadow_importance <- max(agvim[names(agvim)%in%shadow_names])  
  agvim_filtered <- agvim[!(names(agvim)%in%shadow_names)]
  
  df<-data.frame(importance=agvim_filtered, predictor=names(agvim_filtered))
  
  vim$filter <- agvim_filtered>max_shadow_importance
  vim$boruta <- TRUE
  vim$boruta_threshold = max_shadow_importance
  
  return(vim)
}
