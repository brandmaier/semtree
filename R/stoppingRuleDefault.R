stoppingRuleDefault <- function(node, result, control)
{
 	stop.rule <- TRUE	
  
  if (node$p.values.valid) {
    

    
    if (control$bonferroni && !is.null(result$n.comp)) {
      node$p.uncorrected <- node$p
      node$p <- 1-(1-node$p)**result$n.comp
      node$p.numtests <- result$n.comp
      
    }
    
    if (control$report.level > 5) {
      report(paste("Stopping rule applied based on p value ",node$p), 1)
      if (control$bonferroni) {
        report(paste("Uncorrected p value was ",node$p.uncorrected), 2)       
      }
    }
    
    stop.rule <- node$p > control$alpha;
    
  } else {
    stop.rule <- node$lr <= 0;
  }
  
  return(list(node=node,stop.rule=stop.rule));
  
}