

plot_growth <- function(x, ...){
  UseMethod("plot_growth", x)
}

plot_growth.default <- function(x, ...){
  if(ncol(x) == 2) x$group <- 1
  ggplot2::ggplot(x, ggplot2::aes_string(x = names(x[1]), 
                                         y = names(x[2]), 
                                         colour = names(x[3]))) + 
    ggplot2::geom_point() + 
    ggplot2::geom_path()
}

plot_growth.MxModel <- function(x, lv = NULL, obs = NULL, timelv = NULL, ...){
  tab <- table_results(x, columns = NULL)
  if(is.null(lv)) lv <- unique(tab$col[tab$matrix == "A"])
  if(is.null(obs)) obs <- unique(tab$row[tab$matrix == "A"])
  if(is.null(timelv)) timelv <- lv[2]
  if(!is.null(names(x@submodels))){
    tmp <- lapply(names(x@submodels), function(i){
      list(x[[i]]$M$values[1, lv],
           x[[i]]$A$values[obs, lv])
    })
  } else {
    tmp <- list(x$M$values[1, lv],
                x$A$values[obs, lv])
  }
  df_plot <- do.call(rbind, lapply(1:length(tmp), function(gnum){
    i = tmp[[gnum]]
    data.frame(
      Time = i[[2]][, timelv],
      Expected = .trajectory(i[[1]], i[[2]]),
      group = paste0(x[[names(x@submodels)[gnum]]]$name,
                     ", N = ",
                     x[[names(x@submodels)[gnum]]]$data$numObs)
    )
  }))
  plot_growth(df_plot) + ggplot2::theme_bw()
}


plot_growth.semtree <- function(x, lv = NULL, obs = NULL, timelv = NULL, ...){
  if(is.null(lv)) lv <- x$model$latentVars
  if(is.null(obs)) obs <- x$model$manifestVars
  if(is.null(timelv)) timelv <- lv[2]
  mean_vals <- x$model$M$labels[1, lv]
  params <- parameters(x)
  mean_vals <- params[mean_vals, ]
  loadings <- x$model$A$values[obs, lv]
  expected <- apply(mean_vals, 2, .trajectory, L = loadings)
  df_plot <- as.data.frame.table(expected)
  df_plot$Time <- loadings[, timelv][df_plot$Var1]
  names(df_plot)[c(2, 3)] <- c("group", "Expected")
  df_plot <- df_plot[c("Time", "Expected", "group")]
  therange <- range(x$model$data$observed[obs], na.rm = TRUE)
  plot_growth(df_plot) +
    ggplot2::scale_y_continuous(limits = therange) +
    ggplot2::theme_bw()
}

.trajectory <- function(m, L){
  colSums(m * t(L))
}
