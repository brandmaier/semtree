#' Prune a SEM Tree or SEM Forest
#' 
#' Returns a new tree with a maximum depth selected by the user. can be used in
#' conjunction with plot commands to view various pruning levels.
#' 
#' The returned tree is only modified by the number of levels for the tree.
#' This function does not reevaluate the data, but provides alternatives to
#' reduce tree complexity. If the user would like to alter the tree by
#' increasing depth, then max.depth option must be adjusted in the
#' \code{\link{semtree.control}} object (provided further splits are able to be
#' computed).
#' 
#' @aliases prune.semtree prune.semforest prune
#' @param object A \code{\link{semtree}} or semforest object.
#' @param \dots Optional parameters, such as \code{max.depth} the maximum depth
#' of each tree, or also \code{num.trees} when pruning a forest.
#' @return Returns a \code{\link{semtree}} object.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}, \code{\link{semtree.control}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
prune <- function(object, ... ) UseMethod("prune")
