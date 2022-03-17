#' @exportS3Method plot partialDependence
plot.partialDependence <-
  function(x,
           parameter = NULL,
           type = "l",
           xlab = NULL,
           ylab = NULL,
           ...)
  {
    if (is.null(parameter)) {
      stop("Please specify argument 'parameter'!")
    }
    
    if (is.null(xlab)) {
      xlab <- x$reference.var
    }
    
    if (is.null(ylab)) {
      ylab <- parameter
    }
    
    is_fac = is.factor(x$samples[[x$reference.var]])
    
    if (!is_fac) {
      ggplot2::ggplot(x$samples,
                      ggplot2::aes_string(x = x$reference.var, y = parameter)) +
        geom_line() + theme_light() + ggtitle("Partial Dependence Plot")
    } else {
      ggplot2::ggplot(
        x$samples,
        ggplot2::aes_string(x = x$reference.var, y = parameter,
                            fill = x$reference.var)
      ) +
        geom_bar(stat = "identity") +
        theme_light() + ggtitle("Partial Dependence Plot")
    }
  }
