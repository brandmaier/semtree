#'
#' A helper function to create multi-group models
#' in lavaan with arbitrary equality constraints
#' from a fitted lavaan model
#' 
#' @returns negative two log-likelihood of the fitted model given the constraints
#' 
#' @noRd
lav_multigroup <- function(model, subset1, subset2, constraints) {
  
  stopifnot(is(subset1, "data.frame"))
  stopifnot(is(subset2, "data.frame"))
  stopifnot(is(model, "lavaan"))
  
  if (any(is.na(match(constraints, lavaan::partable(model)$label)))) {
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
  offset <- max(row_ids_lower)
  max_free_id <- max(jpart$free[row_ids_lower]) + 1
  for (rid in row_ids_lower) {
    cur_plabel <- jpart[rid, "plabel"]
    cur_label <- jpart[rid, "label"]
    # adjust plabel
    jpart[offset + rid, "plabel"] <-
      ifelse(is.na(cur_plabel), NA, ifelse(cur_plabel %in% constraints, cur_plabel, paste0(grpname, "_", cur_plabel)))
    # adjust label
    jpart[offset + rid, "label"] <- 
      ifelse(is.na(cur_label), NA, ifelse(cur_label %in% constraints, cur_label, paste0(grpname, "_", cur_label)))
    if (cur_label %in% constraints) {
      jpart[offset + rid, "free"] <- jpart[rid, "free"]
    } else {
      if (jpart[rid,"free"] == 0) {
        jpart[offset + rid, "free"] <- 0
      } else {
        jpart[offset + rid, "free"] <- max_free_id
        max_free_id <- max_free_id + 1
      }
    }
      
  }

  jpart$id <- 1:nrow(jpart)
  # (3) add equality constraints explicitly (not necessary?!)
  
  for (pname in constraints) {
    # find the identical pairs (TODO: this could be more than one!)
    eqnames <- jpart$plabel[jpart$label==pname]
    if (length(eqnames) != 2) stop("Not implemented! Probably you have constraints within a model")
    row <- data.frame(id=nrow(jpart)+1, lhs=eqnames[1], op="==",
                      rhs=eqnames[2],user=2,block=0,group=0,
                      free=0,ustart=NA,exo=0,start=0,est=0,se=0,
                      label="", plabel="")
    jpart <- rbind(jpart, row)
  }
  
  # only for debugging - delete soon
  #pt<-parTable(lavaan(model=raw_model,data=joinset, group=grpname, do.fit=TRUE))
  # 1:25 26:50 + 51:60 constraints on free params
  
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