#' @title Plot parameter differences
#' @description Visualizes parameter differences between post-split nodes with
#' different plot types.
#' @param tree a semtree object.
#' @param plot a character that specifies the plot typ. Available plot types
#' are "ballon" (default), "heatmap", and "bar".
#' @param measure a character. "raw" (default) gives the absolut values of the
#' parameter differences. "wald" gives the squared parameter differences devided
#' by their pooled standard errors. "test" gives the contributions of the
#' parameters to the test statistic.
#' @param normalize logical value; if TRUE parameter differences of each split
#' are divided by sum of all differences the corresponding split. Set to FALSE
#' by default.
#' @param title logical value; if TRUE a title is added to the plot.
#' @param structure logical value; if TRUE the structure of the tree is plotted
#' on the right side.
#' @author Manuel Arnold
#' @export

plotParDiff <- function(tree, plot = "ballon", measure = "raw",
                        normalize = FALSE, title = TRUE, structure = FALSE) {
  
  par_diff <- getParDiff(tree = tree, measure = measure, normalize = normalize)
  
  df <- as.data.frame(matrix(NA, nrow = nrow(par_diff) * ncol(par_diff),
                             ncol = 3))
  colnames(df) <- c("Node", "Parameter", "Value")
  
  for (i in 1:nrow(par_diff)) {
    for (j in 1:ncol(par_diff)) {
      df[((i-1)*ncol(par_diff)+1):(i*ncol(par_diff)), 1] <- rownames(par_diff)[i]
      df[(i-1)*ncol(par_diff)+j, 2] <- colnames(par_diff)[j]
      df[(i-1)*ncol(par_diff)+j, 3] <- par_diff[i, j]
    }
  }
  
  df$Parameter <- factor(df$Parameter, ordered = TRUE, levels = tree$param_names)
  
  # ballon
  if (plot == "ballon") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Node, y = Parameter)) +
      ggplot2::geom_point(ggplot2::aes(size = Value), col = "red") +
      ggplot2::guides(size = ggplot2::guide_legend(switch(measure,
                                                          "raw" = "Difference",
                                                          "wald" = "Wald Diff.",
                                                          "test" = "Contribution"))) +
      ggplot2::theme_bw()
  }
  
  # heatmap
  if (plot == "heatmap") {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Node, Parameter)) +
      ggplot2::geom_tile(ggplot2::aes(fill = Value)) +
      ggplot2::scale_fill_gradient(low = "white", high = "red") +
      ggplot2::geom_text(ggplot2::aes(label = round(Value, 3))) +
      ggplot2::guides(fill = ggplot2::guide_legend(switch(measure,
                                                          "raw" = "Difference",
                                                          "wald" = "Wald Diff.",
                                                          "test" = "Contribution"))) +
      ggplot2::theme_bw()
  }
  
  # stacked barplots 
  if (plot == "bar") {
    p <- ggplot2::ggplot(df, ggplot2::aes(fill = Parameter, y = Value,
                                          x = Node)) + 
      geom_bar(position = "stack", stat = "identity") +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::guides(fill = ggplot2::guide_legend(switch(measure,
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
  
  if (structure) {
    graphics::par(mfrow = c(1, 2))
    plotTreeStructure(tree)
    graphics::plot.new()
    vps <- gridBase::baseViewports()
    grid::pushViewport(vps$figure)
    vp1 <- grid::plotViewport(c(0, 0, 0, 0))
    print(p, vp = vp1)
  } else {
    print(p)
  }
  
}
