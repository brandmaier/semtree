#' @title Retain only basic tree structure
#' @description Removes all elements of a \code{semforest} or \code{semtree}
#' except for the tree structure and terminal node parameters. This is to
#' reduce the heavy memory footprint of sem trees and forests.
#' @param x An object for which a method exists.
#' @param parameters Character vector, referencing parameters in the SEM model.
#' Defaults to \code{NULL}, in which case all free model parameters are
#' returned.
#' @return List
#' @details Objects of class \code{semforest} and \code{semtree} are very
#' large, which complicates downstream operations such as making partial
#' dependence plots, or using the model in interactive contexts (like Shiny
#' apps). Running \code{strip} removes all elements of the model
#' except for the tree structure and terminal node parameters. Note that some
#' methods are no longer available for the resulting object - e.g.,
#' \code{\link{varimp}} requires the terminal node SEM models to compute the
#' likelihood ratio.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname strip
#' @export
strip <- function(x, parameters = NULL) {
  UseMethod("strip", x)
}

#' @method strip semtree
#' @export
strip.semtree <- function(x, parameters = NULL) {
  if (inherits(x$model, "lavaan")) {
    sums <- parTable(x$model)
    parlab <- do.call(paste0, sums[2:4])
    parlab[which(!sums$label == "")] <-
      sums$label[which(!sums$label == "")]
    
    if (is.null(parameters)) {
      parameters <- parlab[!sums$free == 0]
      ids <- sums$plabel[!sums$free == 0]
    } else {
      parameters <-
        parameters[parameters %in% parlab | parameters %in% sums$label]
      ids <-
        sums$plabel[parlab %in% parameters | sums$label %in% parameters]
    }
    out <- strip_lav(x, parameters = ids)
  } else {
    if (is.null(parameters)) {
      sums <- summary(x$model)
      parameters <- sums$parameters$name
    }
    out <- strip_mx(x, parameters = parameters)
  }
  attr(out, "parameters") <- parameters
  class(out) <- c("semtree_stripped", class(out))
  out
}

#' @method strip semforest
#' @export
strip.semforest <- function(x, parameters = NULL) {
  if (inherits(x$model, "lavaan")) {
    sums <- parTable(x$model)
    parlab <- do.call(paste0, sums[2:4])
    parlab[which(!sums$label == "")] <-
      sums$label[which(!sums$label == "")]
    
    if (is.null(parameters)) {
      parameters <- parlab[!sums$free == 0]
      ids <- sums$plabel[!sums$free == 0]
    } else {
      parameters <-
        parameters[parameters %in% parlab | parameters %in% sums$label]
      ids <-
        sums$plabel[parlab %in% parameters | sums$label %in% parameters]
    }
    out <- lapply(x$forest, strip_lav, parameters = ids)
  } else {
    if (is.null(parameters)) {
      sums <- summary(x$model)
      parameters <- sums$parameters$name
    }
    out <- lapply(x$forest, strip_mx, parameters = parameters)
  }
  attr(out, "parameters") <- parameters
  class(out) <- c("semforest_stripped", class(out))
  out
}


# Lavaan ------------------------------------------------------------------

strip_lav <- function(x, parameters = NULL) {
  UseMethod("strip_lav", x)
}

strip_lav <- function(x, parameters = NULL) {
  if (x$caption == "TERMINAL") {
    sums <- parameterTable(x$model)
    return(list(parameters = sums$est[match(parameters, sums$plabel)],
                node_id = x$node_id))
  } else {
    return(list(
      rule = x$rule[c("relation", "value", "name")],
      left_child = strip_lav(x = x$left_child, parameters = parameters),
      right_child = strip_lav(x = x$right_child, parameters = parameters)
    ))
  }
}


# OpenMx ------------------------------------------------------------------

strip_mx <- function(x, parameters = NULL) {
  if (x$caption == "TERMINAL") {
    sums <- summary(x$model)
    return(list(
      parameters = sums$parameters$Estimate[match(parameters, sums$parameters$name)],
      node_id = x$node_id
    ))
  } else {
    return(list(
      rule = x$rule[c("relation", "value", "name")],
      left_child = strip(x = x$left_child, parameters = parameters),
      right_child = strip(x = x$right_child, parameters = parameters)
    ))
  }
}

traverse_stripped <- function(row, tree, what = "parameters") {
  if (!is.null(tree[["rule"]])) {
    traverse_stripped(row = row,
                      tree = tree[[c("left_child", "right_child")[(
                        do.call(tree$rule$relation, list(row[[tree$rule$name]], 
                                                         tree$rule$value)) +
                                                                     1)]]],
                      what = what)
  } else {
    return(tree[[what]])
  }
}
