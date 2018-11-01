context("pValueString")

test_that("pValueString", {

  # inputs
  expect_type(pValueString("0.05"), "character")
  expect_type(pValueString(0.05), "character")
  expect_error(pValueString(""))

  # outputs
  expect_match(pValueString("0.03"), ".03")
  expect_match(pValueString(0.03), ".03")
  expect_match(pValueString("0.009"), "< .01")
  expect_match(pValueString(0.009), "< .01")
  expect_match(pValueString("0.0009"), "< .001")
  expect_match(pValueString(0.000), "< .001")

})
