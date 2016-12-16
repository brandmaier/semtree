stopping.rule.default <- function(node, result, control)
{
 	stop.rule <- TRUE	
  
  if (node$p.values.valid) {
    node$p <- pchisq(node$lr,df=node$df, lower.tail=F)
    if (control$bonferroni && !is.null(result$n.comp)) {
      node$p.uncorrected <- node$p
      node$p <- 1-(1-node$p)**result$n.comp
      node$p.numtests <- result$n.comp
      
    }
    stop.rule <- node$p > control$alpha;
    
  } else {
    stop.rule <- node$lr <= 0;
  }
  
  return(list(node=node,stop.rule=stop.rule));
  
}