get.children.map <- function(tree)
{ 
  dict <- list()
  for (i in 1:getNumNodes(tree))
    dict[[i]] <- NA
  dict <- get.children.map.rec(tree, dict, 0)  
  
  # clean up the NAs
  for (i in 1:getNumNodes(tree))
  {
    dict[[i]] <- dict[[i]][!is.na(dict[[i]])] 
  }
  
  return(dict)
}

get.children.map.rec <- function(tree, dict, parent_id)
{
    if (tree$caption == "TERMINAL") {
      dict[[parent_id]] <- tree$node_id
    } else {
      resultl <- get.children.map.rec(tree$left_child, dict, tree$node_id)
      resultr <- get.children.map.rec(tree$right_child, dict, tree$node_id)

      if (parent_id > 0)
      dict[[parent_id]] <- c(tree$node_id, resultl[[tree$node_id]], resultr[[tree$node_id]])
      for (i in 1:length(dict)) {
        dict[[i]] <- c(dict[[i]], resultl[[i]])
        dict[[i]] <- c(dict[[i]], resultr[[i]])
      }
    }
    
    return(dict)
}