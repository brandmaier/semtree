checkControl <- function(control, fail=TRUE){
   
  if(inherits(control,"semtree.control")){
    fail <- check.semtree.control(control)
  }
  if(inherits(control,"semforest.control")){
    fail <- check.semforest.control(control)
  }
  return(fail)
}

check.semtree.control <- function(control, fail=TRUE)
{
 attr <- attributes(control)$names
 def.attr <- attributes(semtree.control())$names
 if ((length(intersect(attr, def.attr)) != length(attr)))
 {
   unknown <- setdiff(attr, def.attr)
   msg <- paste("Control object contains unknown parameters:",unknown)
   if (fail) {
     ui_fail(msg)
     stop()
   } else {
     ui_warn(msg);
     return(FALSE);
   }
 } else {
   
   temp <- semtree.control()
   for ( nms in attributes(temp)$names) {
     val <- control[[nms]]
     if (!all(is.na(val)) && !all(is.na(temp[[nms]]))) {
       if (!(class(val)==class(temp[[nms]]))) {
         warning(paste0("Possibly wrong class for semtree_control option ",nms))
       }
     }
   } # end for
   
   
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
     ui_fail(msg)
      stop()
   } else {
     ui_warn(msg);
     return(FALSE);
   }
 } else {
   return (TRUE);
 }
}