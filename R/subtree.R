subtree <-
  function(tree, startNode=NULL, level=0, foundNode=FALSE)
  {
    if(is.null(startNode)){
      warning("No starting node (startNode) for subtree provided.")
      return(NULL)
    }
    if (startNode==level&level==0){
      warning("Starting .node is the same as a full tree")
      return(tree)
    }
    
    if(tree$caption!="TERMINAL"){
      if(tree$node_id==startNode){
        foundNode <- TRUE
        return(tree)
      }
      else{
        if(!foundNode)
        l <- subtree(tree$left_child, startNode, level+1, foundNode)
        r <- subtree(tree$right_child, startNode, level+1, foundNode)
      }
      if(!is.null(l)){
        return(l)
      }
      else if(!is.null(r)){
        return(r)
      }
    }
  }

# tree <- subtree(result2, startNode=9)
# plot(tree)
