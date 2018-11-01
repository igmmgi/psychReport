context("pValueSummary")

test_that("pValueSummary", {

  # inputs
  expect_error(pValueSummary("a"))
  expect_error(pValueSummary(c(0.1, "a")))

  # outputs
  expect_equal(pValueSummary(0.03), "*")
  expect_equal(pValueSummary(0.003), "**")
  expect_equal(pValueSummary(0.0003), "***")
  expect_equal(pValueSummary(c(0.1, 0.01)), c(".10", "*"))
  expect_equal(pValueSummary(c(0.115, 0.001)), c(".12", "**"))

})
