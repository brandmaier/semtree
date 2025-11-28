surrogate <- function(rule, mydata)
{

# use the current subset and find a surrogate split
# (1) create a new column in the subset
value <- mydata[, rule$name]
mydata[, rule$name] <- as.integer(do.call(rule$relation, list(value, rule$value)))



# (2) find best predictor for "splitter" among all variables

result <- rpart::rpart(
  data = mydata,
  formula = as.formula(paste0(rule$name, "~."))
)


}

N <- 1000
  y <- rnorm(N)
  
  modelData <- data.frame(y)
  
  for (i in 1:10) {
    modelData[,paste0("x",i)] <- 
      factor(sample(c(0,1,2,3,4),N,replace=TRUE),ordered=TRUE)
  }
  
rule <- list(name="x2", relation=">", value="1")

surrogate(rule = rule, mydata = modelData)    
