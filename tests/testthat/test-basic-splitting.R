# Basic splitting behavior across predictor types.

testthat::skip_on_cran()

generate_base_covariates <- function(n) {
  list(
    var_numeric = seq_len(n),
    var_ordered = ordered(sample(c(1, 2, 3, 4), size = n, prob = rep(0.25, 4), replace = TRUE)),
    var_ordered_named = ordered(
      sample(c(1, 2, 3, 4), size = n, prob = rep(0.25, 4), replace = TRUE),
      labels = c("one", "two", "three", "four")
    ),
    var_unordered_named = factor(
      sample(c("red", "green", "blue", "teal with a little bit of rosé"),
        size = n,
        prob = rep(0.25, 4),
        replace = TRUE
      )
    )
  )
}

fit_tree <- function(data, control = semtree_control()) {
  fitted_model <- lavaan::lavaan("x ~~ x", data = data)
  semtree(fitted_model, data = data, control = control)
}

expect_tree_class <- function(tree) {
  expect_equal(class(tree), "semtree")
}

n <- 500
set.seed(9358)
covariates <- generate_base_covariates(n)

testthat::test_that("ordered named factors yield a split on the manipulated level", {
  set.seed(233453)
  x <- rnorm(n)
  x <- x * ifelse(covariates$var_ordered_named == "one", 0.5, 10)

  tree <- fit_tree(data.frame(x = x, var_ordered_named = covariates$var_ordered_named))

  expect_tree_class(tree)
  expect_gte(getDepth(tree), 1)
  expect_equal(tree$rule$value, "one")
})

testthat::test_that("unordered named factors choose the manipulated category", {
  set.seed(3490843)
  x <- rnorm(n)
  x <- x * ifelse(covariates$var_unordered_named == "green", 1, 10)

  tree <- fit_tree(
    data.frame(x = x, var_unordered_named = covariates$var_unordered_named),
    control = semtree_control(verbose = FALSE, report.level = 99)
  )

  expect_tree_class(tree)
  expect_gte(getDepth(tree), 1)
  expect_equal(as.character(tree$rule$value), "green")
})

testthat::test_that("ordered numeric predictors split at the expected threshold", {
  set.seed(2333463)
  x <- rnorm(n)
  x <- x * ifelse(covariates$var_ordered <= 2, 0.5, 10)

  tree <- fit_tree(
    data.frame(x = x, var_ordered = covariates$var_ordered),
    control = semtree_control(max.depth = 3)
  )

  expect_tree_class(tree)
  expect_equal(getDepth(tree), 2)
  expect_equal(tree$caption, "var_ordered > 2")
})

testthat::test_that("numeric predictors split near the engineered mean difference", {
  set.seed(23334653)
  x <- rnorm(n)
  x <- x * ifelse(covariates$var_numeric < mean(covariates$var_numeric), 0.5, 10)

  tree <- fit_tree(
    data.frame(x = x, var_numeric = covariates$var_numeric),
    control = semtree_control(max.depth = 1)
  )

  expect_equal(getDepth(tree), 1)
  expect_equal(tree$caption, "var_numeric >= 251.5")
})

testthat::test_that("multiple predictors can produce a deep tree", {
  set.seed(23334653)
  x <- rnorm(n)
  x <- x * ifelse(covariates$var_ordered <= 2, 0.5, 10)

  tree <- fit_tree(
    data.frame(
      x = x,
      var_ordered = covariates$var_ordered,
      var_numeric = covariates$var_numeric,
      var_unordered_named = covariates$var_unordered_named
    ),
    control = semtree_control(verbose = FALSE, report.level = 99)
  )

  expect_tree_class(tree)
  expect_equal(getDepth(tree), 7)
})

testthat::test_that("method = fair supports the same basic split patterns", {
  set.seed(233453)
  x_ordered <- rnorm(n)
  x_ordered <- x_ordered * ifelse(covariates$var_ordered_named == "one", 0.5, 10)

  ordered_tree <- fit_tree(
    data.frame(x = x_ordered, var_ordered_named = covariates$var_ordered_named),
    control = semtree_control(method = "fair")
  )

  expect_tree_class(ordered_tree)
  expect_gte(getDepth(ordered_tree), 1)
  expect_equal(ordered_tree$rule$value, "one")

  set.seed(3490843)
  x_unordered <- rnorm(n)
  x_unordered <- x_unordered * ifelse(covariates$var_unordered_named == "green", 1, 10)

  unordered_tree <- fit_tree(
    data.frame(x = x_unordered, var_unordered_named = covariates$var_unordered_named),
    control = semtree_control(method = "fair")
  )

  expect_tree_class(unordered_tree)
  expect_gte(getDepth(unordered_tree), 1)
  expect_equal(as.character(unordered_tree$rule$value), "green")

  set.seed(23334653)
  x_numeric <- rnorm(n)
  x_numeric <- x_numeric * ifelse(covariates$var_numeric < mean(covariates$var_numeric), 0.5, 10)

  numeric_tree <- fit_tree(
    data.frame(x = x_numeric, var_numeric = covariates$var_numeric),
    control = semtree_control(max.depth = 3, method = "fair")
  )

  expect_equal(numeric_tree$caption, "var_numeric >= 251.5")
})
