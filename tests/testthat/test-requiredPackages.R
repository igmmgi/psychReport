context("requiredPackages")

test_that("requiredPackages", {

  expect_error(requiredPackages(c("ez", "dplyr")), NA)
  expect_error(requiredPackages(c("ez", "dplyr", "xxx")))

})
