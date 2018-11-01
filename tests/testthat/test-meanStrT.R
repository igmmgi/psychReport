context("meanStrT")

test_that("meanStrT", {

  set.seed(1)

  # simulated data for t-test
  comp   <- rtDist(100, 500, 50, 200)
  incomp <- rtDist(100, 550, 50, 200)
  tObj   <- t.test(incomp, comp, paired = TRUE)

  expect_equal(meanStrT(tObj), "54")
  expect_equal(meanStrT(tObj, unit = "ms"), "54 ms")
  expect_equal(meanStrT(tObj, unit = "%"), "54 \\\\%")

  tObj   <- t.test(incomp, comp, paired = FALSE)
  expect_equal(meanStrT(tObj), "553 vs. 499")
  expect_equal(meanStrT(tObj, unit = "ms"), "553 vs. 499 ms")

})
