stoppingRuleDefault <- function(node, result, control)
{
 	stop.rule <- TRUE	
  
  if (node$p.values.valid) {
    
    stop.rule <- node$p > control$alpha;
    
  } else {
    stop.rule <- node$lr <= 0;
  }
  
  return(list(node=node,stop.rule=stop.rule));
  
}