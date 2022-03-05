clear_underbrush <- function(x, parameters){
  UseMethod("clear_underbrush", x)
}

clear_underbrush.semtree <- function(x, parameters){
  browser()
  if(x$caption == "TERMINAL"){
    sums <- summary(x$model)
    return(list(parameters = sums$parameters$Estimate[match(parameters, sums$parameters$name)]))
  } else {
    return(list(rule = x$rule,
                left_child = clear_underbrush(x = x$left_child, parameters = parameters),
                right_child = clear_underbrush(x = x$right_child, parameters = parameters)))
  }  
}

clear_underbrush.semforest <- function(x, parameters = NULL){
  browser()
  if(is.null(parameters)){
    sums <- summary(x$model)
    parameters <- sums$parameters$name
  }
  out <- lapply(forest$forest, clear_underbrush, parameters = parameters)
  attr(out, "parameters") <- parameters
  class(out) <- c("semforest_light", class(out))
  out
}

traverse_light <- function(row, tree){
  if(!is.null(tree[["parameters"]])){
    return(tree[["parameters"]])
  } else {
    traverse_light(row = row,
                   tree = tree[[c("left_child", "right_child")[(do.call(tree$rule$relation, list(row[[tree$rule$name]], tree$rule$value))+1)]]])
  }
}
