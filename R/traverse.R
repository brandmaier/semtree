traverse.rec <- function(row, tree)
{

  if (tree$caption == "TERMINAL")
    return(tree$node_id)
  
  rule <- tree$rule
  
  value <- tryCatch({
    row[[rule$name]]
  }, error = function(cond) {
    message("ERROR! Incomplete dataset!")
    stop()
    return(NA)
  })
  
  if (is.na(value)) {
    if (is.null(tree$missing.model)) {
      
      if (is.null(tree$rule_surrogates)) return(tree$node_id)
      else {
        i <- 1
        while(is.na(value)) {
          rule = tree$rule_surrogates[[i]]
          value = row[[rule$name]]
          i <- i + 1
          if (i > length(tree$rule_surrogates)) return(tree$node_id)
        }  
      }
      
    } else {
      value = predict(tree$missing.model, newdata = row)
    }
    
    
    
  }

  log.val = do.call(rule$relation, list(value, rule$value))
  
  if (!log.val)
  {
    return(traverse.rec(row, tree$left_child))
    
  } else {
    return(traverse.rec(row, tree$right_child))
    
    
  }
  
}

traverse <- function(tree, dataset)
{
  if (!is.null(tree$traverse.fun)) {
    return(tree$traverse.fun(dataset))
  }
  
  if (is(dataset, "data.frame")) {
    result <- rep(NA, dim(dataset)[1])
    for (i in 1:dim(dataset)[1]) {
      result[i] <- traverse.rec(row = dataset[i, ], tree = tree)
    }
    return(result)
  } else {
    return(apply(
      X = dataset,
      MARGIN = 1,
      FUN = traverse.rec,
      tree
    ))
  }
}
