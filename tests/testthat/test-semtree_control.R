skip_on_cran()


testthat::test_that("control object is created and checked correctly", {
  ctrl <- semtree::semtree.control(min.N = NULL)
  chck <- semtree:::check.semtree.control(ctrl)
  expect_true(chck)
}  )
