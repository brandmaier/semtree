skip_on_cran()

library(lavaan)
library(semtree)
set.seed(2345)

n = 200
var_unordered_named <- factor(sample(c("red","green","blue","teal","purple"),
                                                                          size=n, replace = TRUE))




x <- rnorm(n)
x <- x + ifelse( var_unordered_named=="green" , 20, 1)
df <- data.frame(x, var_unordered_named)

model = "x ~~ x; x~mu*0"
fitted_model <- lavaan(model, df)


tree_fair = semtree(fitted_model, df, control=semtree_control(method="fair"))
tree_score = semtree(fitted_model, df, control=semtree_control(method="score"))
tree_naive = semtree(fitted_model, df, control=semtree_control(method="naive"))

plot(tree_score)
plot(tree_naive)
plot(tree_fair)

test_that("first split is identical across methods and correct",
          {
            expect_true(getDepth(tree_score)==2)
            expect_true(getDepth(tree_fair)==2)
            expect_true(getDepth(tree_naive)==2)
            expect_true(all(tree_fair$rule$value== tree_naive$rule$value))
            expect_true(all(tree_fair$rule$value== tree_score$rule$value))
            expect_true(tree_naive$rule$value=="green")
          })

