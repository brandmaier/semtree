plot_growth <- function(x, ...){
  UseMethod("plot_growth", x)
}

plot_growth.default <- function(x, ...){
  if(ncol(x) == 2) x$group <- 1
  ggplot2::ggplot(x, ggplot2::aes_string(x = names(x)[2], 
                                         y = names(x)[3], 
                                         colour = names(x)[1])) + 
    ggplot2::geom_point() + 
    ggplot2::geom_path() +
    ggplot2::theme_bw()
}

.trajectory <- function(m, L){
  colSums(m * t(L))
}
