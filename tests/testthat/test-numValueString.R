context("numValueString")

test_that("numValueString", {

  expect_equal(numValueString(100.321, numDigits = 2, unit = "ms"), "100.32 ms")
  expect_equal(numValueString(100.329, numDigits = 2, unit = "ms"), "100.33 ms")
  expect_equal(numValueString(100.329, numDigits = 0, unit = "ms"), "100 ms")
  expect_equal(numValueString( 80.256, numDigits = 1, unit = "%"),  "80.3 \\%")
  expect_equal(numValueString( 4.129,  numDigits = 2, unit = "mv"), "4.13 $\\mu$V")
  expect_error(numValueString( 4.129,  numDigits = 2, unit = "abc"))

})
