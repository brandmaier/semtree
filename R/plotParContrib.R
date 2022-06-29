#' @title Plot a heatmap of the parameter contributions to the score-based test
#' statistics
#' @description Plots a heatmap with the standardized contributions of the model
#'  parameters to the score-based test statistics.
#' @param tree a semtree object.
#' @author Manuel Arnold
#' @export

plotParContrib <- function(tree) {
  
  stopifnot("Score-guided SEM tree required" = tree$control$method == "score")
  
  par_contrib <- getParContrib(tree = tree, standardized = TRUE)
  
  par_contrib <- t(par_contrib)
  
  df <- as.data.frame(matrix(NA, nrow = nrow(par_contrib) * ncol(par_contrib),
                             ncol = 3))
  colnames(df) <- c("Node", "Parameter", "Contribution")
  
  for (i in 1:nrow(par_contrib)) {
    for (j in 1:ncol(par_contrib)) {
      df[((i-1)*ncol(par_contrib)+1):(i*ncol(par_contrib)), 1] <- rownames(par_contrib)[i]
      df[(i-1)*ncol(par_contrib)+j, 2] <- colnames(par_contrib)[j]
      df[(i-1)*ncol(par_contrib)+j, 3] <- par_contrib[i, j]
    }
  }
  
  ggplot2::ggplot(df, ggplot2::aes(x = df$Node, df$Parameter)) +
    ggplot2::geom_tile(ggplot2::aes(fill = df$Contribution)) +
    ggplot2::scale_fill_gradient(low = "white", high = "red") +
    ggplot2::geom_text(ggplot2::aes(label = round(df$Contribution, 3))) +
    ggplot2::theme_bw()
  
}
