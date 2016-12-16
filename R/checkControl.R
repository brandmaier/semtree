checkControl <- function(control, fail=T){
  if(inherits(control,"semtree.control")){
    fail <- check.semtree.control(control)
  }
  if(inherits(control,"semforest.control")){
    fail <- check.semforest.control(control)
  }
  return(fail)
}

check.semtree.control <- function(control, fail=T)
{
 # message("TREE")
 attr <- attributes(control)$names
 def.attr <- attributes(semtree.control())$names
 if ((length(intersect(attr, def.attr)) != length(attr)))
 {
   unknown <- setdiff(attr, def.attr)
   msg <- paste("Control object contains unknown parameters:",unknown)
   if (fail) {
     stop(msg)
   } else {
     warning(msg);
     return(FALSE);
   }
 } else {
   return (TRUE);
 }
 
 return (length(intersect(attr, def.attr)) == length(attr));
}

check.semforest.control <- function(control, fail=T)
{
 # message("FOREST")
 attr <- attributes(control)$names
 def.attr <- attributes(semforest.control())$names
 
 if ((length(intersect(attr, def.attr)) != length(attr)))
 {
   unknown <- setdiff(attr, def.attr)
   msg <- paste("Control object contains unknown parameters:",unknown)
   if (fail) {
     stop(msg)
   } else {
     warning(msg);
     return(FALSE);
   }
 } else {
   return (TRUE);
 }
}