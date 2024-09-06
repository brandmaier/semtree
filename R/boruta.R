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
#' @param maxRuns Maximum number of boruta search cycles
#' @param pAdjMethod A value from \link{stats::p.adjust.methods} defining a
#'          multiple testing correction method
#' @param alpha p-value cutoff for decisionmaking. Default .05
#' @param verbose Verbosity level for boruta processing
#'          similar to the same argument in \link{semtree.control} and
#'          \link{semforest.control}
#' @param \dots Optional parameters to undefined subfunctions
#' @return A vim object with several elements that need work.
#'          Of particular note, `$importance` carries mean importance;
#'          `$decision` denotes Accepted/Rejected/Tentative;
#'          `$impHistory` has the entire varimp history; and
#'          `$details` has exit values for each parameter.
#' @author Priyanka Paul, Timothy R. Brick, Andreas Brandmaier
#' @seealso \code{\link{semtree}} \code{\link{semforest}}
#'
#' @keywords tree models multivariate
#' @export
boruta <- function(model,
                   data,
                   control = NULL,
                   predictors = NULL,
                   maxRuns = 30,
                   pAdjMethod = "none",
                   alpha = .05,
                   verbose = FALSE,
                   ...) {
  # detect model (warning: duplicated code)
  if (inherits(model, "MxModel") || inherits(model, "MxRAMModel")) {
    tmp <-
      getPredictorsOpenMx(mxmodel = model,
                          dataset = data,
                          covariates = predictors)
    model.ids <- tmp[[1]]
    covariate.ids <- tmp[[2]]
  } else if (inherits(model, "lavaan")) {
    tmp <-
      getPredictorsLavaan(model, dataset = data, covariates = predictors)
    model.ids <- tmp[[1]]
    covariate.ids <- tmp[[2]]
  } else {
    ui_stop("Unknown model type selected. Use OpenMx or lavaanified lavaan models!")
    
  }
  
  # initial checks
  # Checks on x & y from the boruta package
  if (length(grep('^shadow', covariate.ids) > 0))
    stop(
      'Attributes with names starting from "shadow" are reserved for internal use. Please rename them.'
    )
  if (maxRuns < 11)
    stop('maxRuns must be greater than 10.')
  if (!pAdjMethod %in% stats::p.adjust.methods)
    stop(c(
      'P-value adjustment method not found. Must be one of:',
      stats::p.adjust.methods
    ))
  
  # Might clash with some other semtrees stuff
  if (is.null(predictors)) {
    predictors <- names(data)[covariate.ids]
  }
  
  # Initialize and then loop over runs:
  impHistory <-
    data.frame(matrix(NA, nrow = 0, ncol = length(predictors) + 3))
  names(impHistory) <-
    c(predictors, "shadowMin", "shadowMean", "shadowMax")
  decisionList <-
    data.frame(
      predictor = predictors,
      decision = "Tentative",
      hitCount = 0,
      raw.p = NA,
      adjusted.p = NA
    )
  
  # TODO: Parallelize the first five runs.
  for (runNo in 1:maxRuns) {
    if (verbose) {
      message(paste("Beginning Run", runNo))
    }
    
    # stage 1 - create shadow features
    rejected <-
      decisionList$predictor[decisionList$decision == "Rejected"]
    current.predictors <- setdiff(predictors, rejected)
    current.covariate.ids <-
      setdiff(covariate.ids, names(data) %in% rejected)
    current.data <- data[, setdiff(names(data), rejected)]
    
    shadow.ids <-
      (ncol(current.data) + 1):(ncol(current.data) + length(current.covariate.ids))
    
    for (cur_cov_id in current.covariate.ids) {
      # pick column and shuffle
      temp_column <- current.data[, cur_cov_id]
      temp_column <-
        sample(temp_column, length(temp_column), replace = FALSE)
      # add to dataset as shadow feature
      temp_colname <-
        paste0("shadow_", names(current.data)[cur_cov_id], collapse = "")
      current.data[temp_colname] <- temp_column
      if (!is.null(current.predictors))
        current.predictors <- c(current.predictors, temp_colname)
    }
    
    # TODO: Pre-run model if needed.
    
    # run the forest
    forest <-
      semforest(model, current.data, control, current.predictors, ...)
    
    # run variable importance
    vim <- varimp(forest)
    
    # get variable importance from shadow features
    shadow_names <- names(current.data)[shadow.ids]
    agvim <- aggregateVarimp(vim, aggregate = "mean")
    
    # Compute shadow stats
    shadow_importances <- agvim[names(agvim) %in% shadow_names]
    impHistory[runNo, "shadowMax"] <-
      max_shadow_importance <- max(shadow_importances)
    impHistory[runNo, "shadowMin"] <- min(shadow_importances)
    impHistory[runNo, "shadowMean"] <- mean(shadow_importances)
    agvim_filtered <- agvim[!(names(agvim) %in% shadow_names)]
    impHistory[runNo, names(agvim_filtered)] <- agvim_filtered
    
    # Compute "hits"
    hits <-
      decisionList$predictor %in% names(agvim_filtered[agvim_filtered > max_shadow_importance])
    decisionList$hitCount[hits] <- decisionList$hitCount[hits] + 1
    
    # Run tests.
    # The biasing here means that there are no decisions without correction
    #   before 5 runs and no decisions with Bonferroni before 7 runs.
    
    # Run confirmation tests (pulled from Boruta package)
    newPs <-
      stats::pbinom(decisionList$hitCount - 1, runNo, 0.5, lower.tail = FALSE)
    adjPs <- stats::p.adjust(newPs, method = pAdjMethod)
    acceptable <- adjPs < alpha
    updateList <- acceptable & decisionList$decision == "Tentative"
    decisionList$raw.p[updateList] <- newPs[updateList]
    decisionList$adjusted.p[updateList] <- adjPs[updateList]
    decisionList$decision[updateList] <- "Confirmed"
    
    # Run rejection tests (pulled from Boruta package)
    newPs <-
      stats::pbinom(decisionList$hitCount, runNo, 0.5, lower.tail = TRUE)
    adjPs <- stats::p.adjust(newPs, method = pAdjMethod)
    acceptable <- adjPs < alpha
    updateList <- acceptable & decisionList$decision == "Tentative"
    decisionList$raw.p[updateList] <- newPs[updateList]
    decisionList$adjusted.p[updateList] <- adjPs[updateList]
    decisionList$decision[updateList] <- "Rejected"
    
    if (!any(decisionList$decision == "Tentative")) {
      break
    }
    
  }
  
  vim$importance <- colMeans(impHistory, na.rm = TRUE)
  vim$impHistory <- impHistory
  vim$decisions <- decisionList$decision
  vim$details <- decisionList
  
  vim$filter <-
    decisionList$decision == "Confirmed"  # Turns into hitreg
  vim$boruta <- TRUE
  
  class(vim) <- "boruta"
  
  # TODO: Loop ends here with some reporting.
  
  return(vim)
}


#' @exportS3Method plot boruta
plot.boruta = function(vim, type = 0, ...) {
  decisionList = vim$details
  impHistory = vim$impHistory
  impHistory <- impHistory |>
    dplyr::mutate(rnd = 1:nrow(impHistory)) |>
    tidyr::pivot_longer(cols = -last_col()) |> #everything()) |>
    dplyr::left_join(data.frame(decisionList),
                     by = dplyr::join_by("name" == "predictor")) |>
    dplyr::mutate(decision =
                    case_when(is.na(decision) ~ "Shadow", .default = decision)) |>
    dplyr::group_by(name) |>
    dplyr::mutate(median_value = median(value, na.rm = TRUE))
  
  if (type == 0) {
    ggplot2::ggplot(impHistory,
                    ggplot2::aes(
                      x = stats::reorder(name, median_value),
                      y = value,
                      color = decision
                    )) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab("") +
      ggplot2::ylab("Importance") +
      ggplot2::scale_color_discrete(name = "Decision") +
      ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else if (type == 1) {
    ggplot2::ggplot(impHistory,
                    ggplot2::aes(
                      x = rnd,
                      y = value,
                      group = name,
                      col = name
                    )) +
      ggplot2::geom_line() +
      ggplot2::geom_hline(aes(yintercept = median_value, col = name), lwd =
                            2) +
      ggplot2::xlab("Round") +
      ggplot2::ylab("Importance") +
      ggplot2::scale_color_discrete(name = "Predictor")
  } else {
    stop("Unknown graph type. Please choose 0 or 1.")
  }
  
}