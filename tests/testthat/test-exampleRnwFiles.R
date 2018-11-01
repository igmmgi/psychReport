context("exampleRnwFiles")

test_that("exampleRnwFiles", {

  # testthat::expect_error(exampleRnwFiles(1), NA)
  # testthat::expect_error(exampleRnwFiles(2), NA)
  expect_error(exampleRnwFiles(3))

})
