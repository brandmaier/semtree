#'
#' A helper function to create multi-group models
#' in lavaan with arbitrary equality constraints
#' from a fitted lavaan model
#' 
#' @returns negative two log-likelihood of the fitted model given the constraints
lav_multigroup <- function(model, subset1, subset2, constraints) {
  
  stopifnot(is(subset1, "data.frame"))
  stopifnot(is(subset2, "data.frame"))
  stopifnot(is(model, "lavaan"))
  
  if (any(is.na(match(constraints, partable(model)$label)))) {
    stop("Constraints are not labeled parameters in the model specification!")
  }
  
  # (1) combine both datasets and add a grouping variable
  grpname <- generate_unique_colname(subset1)
  subset1[[grpname]] <- 0
  subset2[[grpname]] <- 1
  joinset <- rbind(subset1, subset2)
  
  # (2) manually create a multi-group partable
  
  # (2a) double the parameter table
  jpart <- rbind(lavaan::partable(model), lavaan::partable(model))
  # (2b) update block & group variables
  pgrp <-
    c(rep(1, nrow(lavaan::partable(model))), rep(2, nrow(lavaan::partable(model))))
  jpart$group <- pgrp
  jpart$block <- pgrp
  
  # (2c) update plabels to make all parameters unique
  # unless they are mentioned in constraints
  row_ids_lower <- 1:(nrow(jpart) / 2)
  row_ids_upper <- (nrow(jpart) / 2 + 1):nrow(jpart)
  jpart[row_ids_upper, "plabel"] <-  sapply(jpart[row_ids_lower, "plabel"], function(x) {
    ifelse(is.na(x), NA, ifelse(x %in% constraints, x, paste0(grpname, "_", x)))
  })
  jpart[row_ids_upper, "label"] <-  sapply(jpart[row_ids_lower, "label"], function(x) {
    ifelse(is.na(x), NA, ifelse(x %in% constraints, x, paste0(grpname, "_", x)))
  })
  max_free_id <- max(jpart$free[row_ids_lower]) + 1
  jpart[row_ids_upper, "free"] <- sapply(jpart[row_ids_lower, "free"], function(x) {
    if (x %in% constraints)
      return(x)
    ifelse(x == 0, 0, max_free_id)
    max_free_id <<- max_free_id + 1
  })
  # (3) add equality constraints explicitly (not necessary?!)
  
  for (pname in constraints) {
    eqnames <- jpart$plabel[jpart$label==pname]
    row <- data.frame(id=nrow(jpart)+1, lhs=eqnames[1], op="==",
                      rhs=eqnames[2],user=2,block=0,group=0,
                      free=0,ustart=NA,exo=0,start=0,est=0,se=0,
                      label="", plabel="")
    jpart <- rbind(jpart, row)
  }
  
  # (4) run the model
  
  mg_fit <-
    try(suppressWarnings(eval(parse(
      text = paste(
        model@Options$model.type,
        '(jpart,data=joinset, group = grpname,missing=\'',
        model@Options$missing,
        '\')',
        sep = ""
      )
    ))), silent = T)
  
  #partable(mg_fit)

  # (5) extract the likelihood
  LL.sum <- -2 * lavaan::logLik(mg_fit)
  
  return(LL.sum)
}