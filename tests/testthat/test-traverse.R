

library(lavaan)

skip_on_cran()

set.seed(5035)

N <- 200

x <- rnorm(N)
y <- rnorm(N)

pred1 <- ordered( sample(c(0,1,2),N,replace=TRUE) )
pred2 <- sample(0:10, N, replace=TRUE)

y2 <- ifelse(pred2>5,0.2*y+0.8*x,x)              

model <- "x~~y; x~~x;y~~y"

df <- data.frame(x,y=y2,pred1,pred2)

fitted_model <- lavaan(model,df)
tree <- semtree(fitted_model, df)
plot(tree)

node_ids = semtree:::traverse(tree, df)

node_ids_correct_top10 <- c(6,2,5,2,6,2,2,7,2,2)

test_that("traversal works correctly", { expect_equal(node_ids[1:10], node_ids_correct_top10)})

#
# further test for categorical variables
set.seed(3433)
pred3 <- factor(sample(c("red","green","blue","yellow"), N, replace=TRUE))
y2 <- ifelse(pred3=="red" | pred3=="yellow",0.2*y+0.8*x,0.8*y+0.2*x)

model <- "x~~y; x~~x;y~~y"

df <- data.frame(x,y=y2,pred3)

fitted_model <- lavaan(model,df)
tree <- semtree(fitted_model, df)
plot(tree)

node_ids = semtree:::traverse(tree, df)

node_ids_correct_top10 <- c(2,2,3,2,2,2,2,2,3,2)

test_that("traversal works correctly", { expect_equal(node_ids[1:10],
                                                      node_ids_correct_top10)})

test_that("rule is correct", {expect_equal(as.character(tree$rule$value),c("blue","green"))})

# more tests for stripped traversal

tree_stripped <- strip(tree)
traverse_stripped(df[1,],tree_stripped,  what="parameters")
traverse_stripped(df[3,],tree_stripped,  what="parameters")
