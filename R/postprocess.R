postprocess <- function(tree)
{
  if (is.null(tree)) { return(tree); }
  
  if (is.null(tree$caption)) {
    ui_warn("Tree has no caption")
    return(tree);
  }
  
  if (tree$caption=="TERMINAL") {
    return(tree);
  } else {
    
    ok <- TRUE;
    # check whether children are valid
    ok <- !is.null(tree$left_child$model) & !is.null(tree$right_child$model);
    
    if (!ok) {
      tree$caption <- "TERMINAL"
      tree$left_child <- NULL
      tree$right_child <- NULL
    } else {
      tree$left_child <- postprocess(tree$left_child)
      tree$right_child <- postprocess(tree$right_child)
    }
    return(tree);
  }
 
}