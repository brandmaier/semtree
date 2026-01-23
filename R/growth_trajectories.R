plot_growth <- function(x, ...){
  UseMethod("plot_growth", x)
}

plot_growth.default <- function(x, ...){
  if(ncol(x) == 2) x$group <- 1
  
  # weird hack to stop CRAN from complaining about
  # ggplot2 .data[[...]] argument
  .data <- NULL
  
  ggplot2::ggplot(x, ggplot2::aes(x = .data[[names(x)[2]]], 
                                         y = .data[[names(x)[3]]], 
                                         colour = .data[[names(x)[1]]])) + 
    ggplot2::geom_point() + 
    ggplot2::geom_path() +
    ggplot2::theme_bw()
}

.trajectory <- function(m, L){
  colSums(m * t(L))
}
