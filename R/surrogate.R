#'use the current data 'mydata' and find a surrogate split
#' 
#' @param rule a given split rule
#' @param mydata a dataset
#' @param agreement_threshold Numeric. Threshold for the agreement to consider a variable as surrogate
#'
surrogate <- function(rule, mydata, agreement_threshold=.7)
{

results <- make_top_k(k=3)

# (1) create split pattern
value <- mydata[, rule$name]
pattern <- as.integer(do.call(rule$relation, list(value, rule$value)))



# (2) iterate over all possible rules to create best match

for (j in 1:ncol(mydata)) {
  
  best_agree <- -Inf
  best_agree_rule <- NULL
  
  if (names(mydata)[j] == rule$name) next;
  value <- mydata[, j]
  
  candidates <- create_rules(mydata[,j], names(mydata)[j])
  for (candidate_rule in candidates) {
   
    candidate_pattern <- as.integer(do.call(candidate_rule$relation, list(value, candidate_rule$value)))
    
    agree = agreement(candidate_pattern, pattern)
    if (agree > best_agree &&  agree >= agreement_threshold) {
      best_agree = agree
      best_agree_rule = candidate_rule
    }
    
    
  }
  
  #debug: cat("Add rule", best_agree_rule$name," ",best_agree_rule$relation," ",best_agree_rule$value, " with score ",best_agree,"\n")
  # for every variable, add the best one as potential candidate
  if (best_agree != -Inf)
    results$add(score = best_agree, payload = best_agree_rule)
  
}

if (length(results$get_payloads())==0) return(NULL)

return(results$get_payloads())

}

agreement <- function(candidate_pattern, pattern) {
  p0 = sum(pattern==0,na.rm = TRUE)
  majority_agreement <- max(p0, 1-p0)
  
  agreement = sum(candidate_pattern==pattern, na.rm=TRUE) / sum(!is.na(pattern))
  
  
#  adjusted_agreement <- (agreement - majority_agreement) /
#    (1 - majority_agreement)
  
#  return(adjusted_agreement)
  return(agreement)
}


make_top_k <- function(k, largest = TRUE) {
  stopifnot(length(k) == 1, k >= 1)
  
  scores <- numeric(0)
  payloads <- vector("list", 0)
  
  better <- if (largest) {
    function(a, b) a > b
  } else {
    function(a, b) a < b
  }
  
  order_scores <- function(scores) {
    order(scores, decreasing = largest)
  }
  
  add <- function(score, payload) {
    if (length(score) != 1 || is.na(score)) {
      stop("score must be one non-missing number")
    }
    
    # Case 1: not full yet
    if (length(scores) < k) {
      scores <<- c(scores, score)
      payloads <<- c(payloads, list(payload))
      
      ord <- order_scores(scores)
      scores <<- scores[ord]
      payloads <<- payloads[ord]
      
      return(invisible(NULL))
    }
    
    # Case 2: full, and new score is not good enough
    worst_score <- scores[k]
    
    if (!better(score, worst_score)) {
      return(invisible(NULL))
    }
    
    # Case 3: full, and new score enters top k
    pos <- which(better(score, scores))[1]
    
    scores <<- append(scores, score, after = pos - 1)
    payloads <<- append(payloads, list(payload), after = pos - 1)
    
    scores <<- scores[seq_len(k)]
    payloads <<- payloads[seq_len(k)]
    
    invisible(NULL)
  }
  
  get <- function() {
    data.frame(
      rank = seq_along(scores),
      score = scores
    )
  }
  
  get_payloads <- function() {
    payloads
  }
  
  get_all <- function() {
    Map(
      function(score, payload) {
        list(score = score, payload = payload)
      },
      scores,
      payloads
    )
  }
  
  clear <- function() {
    scores <<- numeric(0)
    payloads <<- vector("list", 0)
    invisible(NULL)
  }
  
  list(
    add = add,
    get = get,
    get_payloads = get_payloads,
    get_all = get_all,
    clear = clear
  )
}

create_rules <- function(x, name) {
  rules <- list()
  if (is.numeric(x)) {
    vals <- sort(stats::na.omit(unique(x)))
    for (i in 1:(length(vals)-1) )
      rules[[length(rules)+1]] <- list(name=name, relation=">", value=vals[i])
  } else if (is.ordered(x)) {
    lvs <- levels(x)
    lvs <- lvs[-length(lvs)]
    for (i in lvs)
      rules[[length(rules)+1]] <- list(name=name, relation=">", value=i)    
  } else {
    
  }
  
  return(rules)
}


