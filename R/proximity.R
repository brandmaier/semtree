#' SEM Forest Case Proximity
#' @export
#' @title Compute proximity matrix
#' @description Compute a n by n matrix across all trees in a forest,
#' where n is the number of rows in the
#' data, reflecting the proportion of times two cases ended up in the same
#' terminal node of a tree.
#' @param x An object for which a method exists.
#' @param ... Parameters passed to other functions.
#' @return A matrix with dimensions [i, j] whose elements reflect the proportion
#' of times case i and j were in the same terminal node of a tree. 
#' @author Caspar J. Van Lissa, Andreas M. Brandmaier, John J. Prindle
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @seealso \code{\link{semforest}}, \code{\link{semtree}}
#' 
#' @examples 
#' nodeids <- structure(c(9, 3, 5, 7, 10, 4, 6, 8, 9, 3, 5, 7, 10, 4, 6, 8),
#' .Dim = c(4L, 4L))
#' class(nodeids) <- "semforest_node_id"
#' sims <- proximity(nodeids)
#' dd <- as.dist(1-sims)
#' hc <- hclust(dd)
#' groups <- cutree(hc, 2)
#' @rdname proximity
#' @export
proximity <- function(x, data, ...){
  UseMethod("proximity", x)
}

#' @method proximity semforest
#' @export
proximity.semforest <- function(x, data, ...){
  forest_stripped <- strip(x)
  preds <- predict(forest_stripped, data = x$data, type = "node_id")
  proximity(preds)
}
#' @method proximity semforest_stripped
#' @export
proximity.semforest_stripped <- function(x, data, ...){
  preds <- predict(x, data = data, type = "node_id")
  proximity(preds)
}
#' @method proximity semforest_node_id
#' @export
proximity.semforest_node_id <- function(x, data, ...){
  out <- simplify2array(apply(x, 1, function(ids){
    m1 <- matrix(ids, nrow = length(ids), ncol = length(ids))
    m1 == t(m1)
  }, simplify = FALSE))
  apply(out, 1:2, mean)
}