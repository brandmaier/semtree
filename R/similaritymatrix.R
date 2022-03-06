#' @title Make similarity matrix
#' @description Compute a n by n matrix across all trees in a forest,
#' where n is the number of rows in the
#' data, reflecting the proportion of times two cases ended up in the same
#' terminal node of a tree.
#' @param x An object for which a method exists.
#' @param ... Parameters passed to other functions.
#' @return A matrix with dimensions [i, j] whose elements reflect the proportion
#' of times case i and j were in the same terminal node of a tree. 
#' @author Caspar J. Van Lissa
#' @examples 
#' nodeids <- structure(c(9, 3, 5, 7, 10, 4, 6, 8, 9, 3, 5, 7, 10, 4, 6, 8),
#' .Dim = c(4L, 4L))
#' class(nodeids) <- "semforest_node_id"
#' sims <- similarityMatrix(nodeids)
#' dd <- as.dist(1-sims)
#' hc <- hclust(dd)
#' groups <- cutree(hc, 2)
#' @rdname similarityMatrix
#' @export
similarityMatrix <- function(x, data, ...){
  UseMethod("similarityMatrix", x)
}

#' @method similarityMatrix semforest
#' @export
similarityMatrix.semforest <- function(x, data, ...){
  f_light <- clear_underbrush(x)
  preds <- predict(f_light, data = x$data, type = "node_id")
  similarityMatrix(preds)
}
#' @method similarityMatrix semforest_light
#' @export
similarityMatrix.semforest_light <- function(x, data, ...){
  preds <- predict(x, data = data, type = "node_id")
  similarityMatrix(preds)
}
#' @method similarityMatrix semforest_node_id
#' @export
similarityMatrix.semforest_node_id <- function(x, data, ...){
  out <- simplify2array(apply(x, 1, function(ids){
    m1 <- matrix(ids, nrow = length(ids), ncol = length(ids))
    m1 == t(m1)
  }, simplify = FALSE))
  apply(out, 1:2, mean)
}