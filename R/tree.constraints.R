#' SEM Tree Constraints Object
#' 
#' A SEM Tree constraints object holds information regarding specifics on how
#' the tree is grown (similar to the control object). The SEM tree control
#' object holds all information that is independent of a specific model whereas
#' the constraints object holds information that is specific to a certain model
#' (e.g., specifies differential treatment of certain parameters, e.g., by
#' holding them constant across the forest).
#' 
#' 
#' @param local.invariance Vector of parameter names that are locally equal,
#' that is, they are assumed to be equal when assessing a local split but
#' allowed to differ subsequently.
#' @param global.invariance Vector of parameter names that are globally equal,
#' that is, estimated only once and then fixed in the tree.
#' @param focus.parameters Vector of parameter names that exclusively are
#' evaluated for between-group differences when assessing split candidates. If
#' NULL all parameters add to the difference.
#' @author Andreas M. Brandmaier, John J. Prindle
#' @seealso \code{\link{semtree}}
#' @references Brandmaier, A.M., Oertzen, T. v., McArdle, J.J., & Lindenberger,
#' U. (2013). Structural equation model trees. \emph{Psychological Methods},
#' 18(1), 71-86.
#' @export
semtree.constraints <- function(local.invariance=NULL, global.invariance=NULL, focus.parameters=NULL)
{
  ret <- list()
  ret$local.invariance <- local.invariance
  ret$global.invariance <- global.invariance
  ret$focus.parameters <- focus.parameters
  class(ret) <- "semtree.constraints"
  
  return(ret)
  
}

print.semtree.constraints <- function(x, ...) {
  cat("semtree constraints\n")
  cat(" Local invariance ",paste0(x$local.invariance),"\n")
  cat(" Global invariance ",paste0(x$global.invariance),"\n")
  cat(" Focus parameters ",paste0(x$focus.parameter),"\n")
}
