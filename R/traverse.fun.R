traverse.fun.generator <- function(tree)
{
  
  body <- traverse.fun.generator.rec(tree)
  
  code <- paste0('function(row){',body,'}')
  
  return(eval(parse(text=code)))
}

traverse.fun.generator.rec <- function(tree)
{
  if (tree$caption=="TERMINAL") {
    return(paste0('return(',tree$node_id,')'))
  } else {
    # construct if-clause
    pre_block <- paste0('value <- row[["',tree$rule$name,'"]]\n',
                        'if (is.ordered(value)) { value <- as.numeric(as.character(value)) }\n')
    if (tree$rule$relation==">=") {
      cond <- paste0('value >= ',tree$rule$value)
    } else if (tree$rule$relation=="%in%") {
      cond <- paste0('value %in% ', tree$rule$value)
    } else {
      cond <- "stop('Undefined rule encountered!')"
    }
    
    false_block <- traverse.fun.generator.rec(tree$left_child)
    true_block <- traverse.fun.generator.rec(tree$right_child)
    
    return( paste0(pre_block,'if (',cond,')\n {',true_block,'} else {',false_block,'}'))
  }
}

addTraverseFun <- function(tree) {
  tree$traverseRow.fun <- traverse.fun.generator(tree)
  tree$traverse.fun <- function(data) {
    apply(X=data, MARGIN=1, FUN=tree$traverseRow.fun)
  }
  return(tree)
}
