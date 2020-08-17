replaceSymbol <- function(x, original.names, replacement.names) {

  mtch <- which( original.names %in% as.character(x))
#  cat("LM",paste0(length(mtch)),"\n")
  if (length(mtch)>0) {
    x <- as.symbol(replacement.names[mtch])
    cat("Replacing ",paste0(x), " with ", replacement.names[mtch] ,"\n")
  }
  return(x)
}

replaceParametersInMxConstraints<-function(cnst, original.names, replacement.names ) {
  if (length(cnst)==0) return(cnst)
  
  for (i in 1:length(cnst)) {

    cnst[[i]]$formula <- replaceParametersInFormula(cnst[[i]]$formula,
                                                    original.names,
                                                    replacement.names)
  }
  
  return(cnst)
}

# recursively modify constraints objects

replaceParametersInFormula <- function(frm, original.names, replacement.names) {
  
  if (length(frm)==1) {
    frm <- replaceSymbol(frm, original.names, replacement.names)
    return(frm)
  }
  
  # frm are three-tuples with (operator, left, right)
  left.operand <- frm[[2]]
  right.operand <- frm[[3]]
  
  if (is.name(left.operand)) {
    left.operand <- replaceSymbol(left.operand, original.names, replacement.names)
  } else {
    left.operand <- replaceParametersInFormula(left.operand, original.names, replacement.names)
  }
  
  if (is.name(right.operand)) {
    right.operand <- replaceSymbol(right.operand, original.names, replacement.names)
    
  } else {
    right.operand <- replaceParametersInFormula(right.operand, original.names, replacement.names)
    
  }
  
  # set tuple to modified values
  frm[[2]] <-  left.operand
  frm[[3]] <- right.operand
  return(frm)
}