#' @title Plot parameter differences
#' @description Visualizes parameter differences between post-split nodes in a
#' forest with boxplots.
#' @param forest a semforest object.
#' @param plot a character that specifies the plot typ. Available plot types
#' are "boxplot" (default) and "jitter" for a jittered strip plot with mean and
#' standard deviation.
#' @param measure a character. "wald" (default) gives the squared parameter
#' differences devided by their pooled standard errors. "test" gives the
#' contributions of the parameters to the test statistic. "raw" gives the
#' absolute values of the parameter differences.
#' @param normalize logical value; if TRUE parameter differences of each split
#' are divided by sum of all differences the corresponding split. Set to FALSE
#' by default.
#' @param predictors a character. Select predictors that are to be plotted.
#' @param title logical value; if TRUE a title is added to the plot.
#' @author Manuel Arnold
#' @export

plotParDiffForest <- function(forest, plot = "boxplot", measure = "wald",
                              normalize = FALSE, predictors = NULL,
                              title = TRUE) {
  
  par_diff <- getParDiffForest(forest = forest, measure = measure,
                               normalize = normalize)
  
  if (!is.null(predictors)) {
    par_diff <- lapply(par_diff, FUN = function(x) x[x$predictor %in% predictors, , drop = FALSE])
  }
  
  par_diff <- do.call(rbind, par_diff)
  n_parameters <- ncol(par_diff) - 1
  df <- data.frame(matrix(NA, nrow = nrow(par_diff) * n_parameters ,
                          ncol = 3))
  colnames(df) <- c("Predictor", "Parameter", "Value")
  n_parameters <- ncol(par_diff) - 1
  
  for (i in 1:nrow(par_diff)) {
    for (j in 2:ncol(par_diff)) {
      df[((i-1)*n_parameters+1):(i*n_parameters), 1] <- par_diff[i, 1]
      df[(i-1)*n_parameters+(j-1), 2] <- colnames(par_diff)[j]
      df[(i-1)*n_parameters+(j-1), 3] <- par_diff[i, j]
    }
  }
  
  df$Parameter <- factor(df$Parameter, ordered = TRUE,
                         levels = forest$forest[[1]]$param_names)
  df$Predictor <- factor(df$Predictor, ordered = TRUE,
                         levels = forest$covariates)
  
  # stupid fix to stop CRAN from complaining (one possible way out could be aes_string())
  Parameter <- NULL
  Node <- NULL
  Value <- NULL
  
  # ballon
  if (plot == "boxplot") {
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Parameter, y = Value)) +
    ggplot2::geom_boxplot(ggplot2::aes(size = Value), col = "red") +
    ggplot2::facet_grid(Predictor ~ .) +
    ggplot2::guides(size = ggplot2::guide_legend(switch(measure,
                                                        "raw" = "Difference",
                                                        "wald" = "Wald Diff.",
                                                        "test" = "Contribution"))) +
    ggplot2::theme_bw()
  }
  
  # jittered strip plot
  if (plot == "jitter") {
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Parameter, y = Value)) +
      ggplot2::geom_jitter(position = ggplot2::position_jitter(0.2)) +
      ggplot2::stat_summary(fun.data = ggplot2::mean_sdl, geom = "pointrange",
                            color = "red") +
      ggplot2::facet_grid(Predictor ~ .) +
      ggplot2::guides(size = ggplot2::guide_legend(switch(measure,
                                                          "raw" = "Difference",
                                                          "wald" = "Wald Diff.",
                                                          "test" = "Contribution"))) +
      ggplot2::theme_bw()
  }
  
  # add title
  if (title) {
    title_string <- switch(measure,
                           "raw" = "Raw Differences",
                           "wald" = "Wald Differences",
                           "test" = "Contributions to Test Statistic")
    if (normalize) {title_string <- paste("Normalized", title_string)}
    p <- p + ggplot2::ggtitle(title_string)
  }
  
  print(p)
  
}
