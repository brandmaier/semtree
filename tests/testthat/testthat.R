# skip long running tests on CRAN
skip_on_cran()

library(testthat)
test_check("semtree")
