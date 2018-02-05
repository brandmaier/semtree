getNodeList <- function(x)
{
 # if (!inherits(x, "semtree")) stop("x is not of type semtree!")
 # print(x$caption)
  if (x$caption=="TERMINAL") {
    #x$model$node_id <- x$node_id
    return(list(x))
  } else {
    #if (!leafs.only)
    lc <- x$left_child
    rc <- x$right_child
    x$left_child <- NULL
    x$right_child <- NULL
    class(x) <- "node"
    result <- append(list(x), getNodeList(lc) )
    result <- append(result,getNodeList(rc) )
    return(result)
  }
}


#getModelList <- function(x)
#{
#  raw.list <- getModelList.rec(x)
 # node_ids <- lapply(X = raw.list,FUN = function(x){x$node_id})
  
  #result.list
 # re
#}