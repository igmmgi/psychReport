context("mathString")

test_that("mathString", {

  result <- mathString("2", "1", operation = "+", unit = "ms")
  expect_equal(result, "3 ms")

  result <- mathString("2 bananas", "1 apple", operation = "-", unit = "ms")
  expect_equal(result, "1 ms")

  result <- mathString("2 bananas", "1 apple", operation = "-", unit = "mv")
  expect_equal(result, "1 $\\\\mu$V")

  result <- mathString("922.2567", "621.2134", operation = "+", numDigits = 0, unit = "ms")
  expect_equal(result, "1543 ms")

  result <- mathString("9.27", "6.24", operation = "-", numDigits = 2, unit = "%")
  expect_equal(result, "3.03 \\\\%")

})
