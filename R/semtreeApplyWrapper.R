

semtreeApplyWrapper <- function(data,
                                seed,
                                skip,
                                model,
                                semtree.control,
                                with.error.handler = TRUE,
                                predictors,
                                constraints,
                                logfile = FALSE,
                                ...)
{
  if (!is.na(seed)) {
    cat("Set seed ", seed, " for tree in forest\n")
    set.seed(seed)
  }
  
  if (skip)
    return(NULL)
  
  
  
  result <- NULL
  
  if (with.error.handler) {
    tryCatch({
      result <- semtree(
        model = model,
        data = data$bootstrap.data,
        control = semtree.control,
        predictors = predictors,
        constraints = constraints,
        ...
      )
      
    }, error = function(err) {
      errmsg <- paste(date(), paste(err), paste(traceback()), sep = "\n")
      if (isFALSE(logfile)) {
        ui_error(errmsg)
      } else {
        stopifnot(is(logfile, "character"))
        write(errmsg, file = logfile, append = TRUE)
      }
      
      
      #
      return(NULL)
    })
    
  } else {
    result <-
      semtree(
        model = model,
        data = data$bootstrap.data,
        control = semtree.control,
        predictors = predictors,
        constraints = constraints,
        ...
      )
  }
  
  return(result)
}
