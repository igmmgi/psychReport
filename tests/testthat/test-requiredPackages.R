context("requiredPackages")

test_that("requiredPackages", {

  testthat::expect_error(requiredPackages(c("broom", "dplyr")), NA)
  testthat::expect_error(requiredPackages(c("broom", "dplyr", "xxx")))

})

