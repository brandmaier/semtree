# collect all cut-points
collect.rules <- function(x, exclude) {
  if (x$caption=="TERMINAL") {
    return(NULL)
  }
  
  if (!x$rule$variable %in% exclude) {
  c(list(x$rule),
    collect.rules(x$left_child, exclude),
  collect.rules(x$right_child, exclude))
  } else {
    c(collect.rules(x$left_child, exclude),
      collect.rules(x$right_child, exclude))
  }
}

conditionalSample <- function(tree, dataset, id)
{
permutation.idx <- id
rules <- collect.rules(tree, exclude=id)

num.rules <- length(rules)

# if there are too many rules, sample from them
if (num.rules > 10) {
  rules <- sample(x = rules,size=10,replace = FALSE)
}

# obtain logicals from rules
logicalS <- list()
for (i in 1:length(rules)) {
rule <- rules[[i]]
if (rule$relation=="%in%")
  logicalS[[i]] <- dataset[,rule$variable] %in% rule$value
else if (rule$relation==">=")
  logicalS[[i]] <- dataset[,rule$variable] >= rule$value
else
  stop("Not Implemented!")
}

if (length(logicalS)<=0) {
  stop("ERROR in generating rules for resampling!")
}

initialS <- rep(TRUE, length(logicalS[[1]]))

# generate all combinations and roll!
allcombs <- 2^length(rules)
for (i in 1:allcombs) {
  bit.filter <- as.logical(intToBits(i))[1:num.rules]

  
  tempS <- initialS
  for (j in 1:length(bit.filter)) {
    if (bit.filter[j])
      tempS <- tempS & logicalS[[j]]
    else
      tempS <- tempS & !logicalS[[j]]
  }
  # permute only temp 
  rowids <- 1:nrow(dataset)
  selected.rowids <- rowids[tempS]
  
  if (length(selected.rowids) > 0)
  {  
    dataset[selected.rowids,permutation.idx] <- dataset[sample(selected.rowids),permutation.idx]
  }
  
  
  cat(bit.filter," : ", length(selected.rowids),"\n")
}

return(dataset)
}