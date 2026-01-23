plot_partialDependence <-
  function(x,
           parameter = NULL,
           type = "l",
           xlab = NULL,
           ylab = NULL,
           ...) {
    if (is.null(parameter)) {
      stop("Please specify argument 'parameter'!")
    }

    num_reference_vars <- length(x$reference.var)

    if (num_reference_vars == 1) {
      if (is.null(xlab)) {
        xlab <- x$reference.var
      }

      if (is.null(ylab)) {
        ylab <- parameter
      }

      is_fac <- is.factor(x$samples[[x$reference.var]])

      if (!is_fac) {
        ggplot2::ggplot(
          x$samples,
          ggplot2::aes(x = .data[[x$reference.var]], 
                       y = .data[[parameter]])
        ) +
          ggplot2::geom_line() +
          ggplot2::theme_light() +
          ggplot2::ggtitle("Partial Dependence Plot")
      } else {
        ggplot2::ggplot(
          x$samples,
          ggplot2::aes(
            x = .data[[x$reference.var]],
            y = .data[[parameter]],
            fill = .data[[x$reference.var]]
          )
        ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::theme_light() +
          ggplot2::ggtitle("Partial Dependence Plot")
      }
    } else if (num_reference_vars == 2) {
      is_fac1 <- is.factor(x$samples[[x$reference.var[1]]])
      is_fac2 <- is.factor(x$samples[[x$reference.var[2]]])

      if (is_fac1 && !is_fac2) { # swap factor to second position
        is_fac1 <- is_fac2
        is_fac2 <- TRUE
        temp <- x$reference.var[1]
        x$reference.var[1] <- x$reference.var[2]
        x$reference.var[2] <- temp
      }

      if (!is_fac1) {
        gp <- ggplot2::ggplot(
          x$samples,
          ggplot2::aes_string(x = x$reference.var[1], y = parameter)
        ) +
          ggplot2::geom_line() +
          ggplot2::theme_light() +
          ggplot2::ggtitle("Partial Dependence Plot")
      } else {
        gp <- ggplot2::ggplot(
          x$samples,
          ggplot2::aes_string(
            x = x$reference.var[1],
            y = parameter,
            fill = x$reference.var[1]
          )
        ) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::theme_light() +
          ggplot2::ggtitle("Partial Dependence Plot")
      }

      if (is_fac2) {
        gp <- gp + ggplot2::facet_wrap(x$reference.var[2])
      } else {
        ui_stop("Plots are currently only supported if at least one reference variable is a factor.")
      }

      return(gp)
    } else {
      ui_stop("Plots are supported with only up to 2 reference variables.")
    }
  }
