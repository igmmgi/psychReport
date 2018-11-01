context("tValueString")

test_that("tValueString", {

  # simulated data for t-test
  set.seed(1)
  comp   <- rtDist(100, 500, 50, 200)
  incomp <- rtDist(100, 550, 50, 200)
  tObj   <- t.test(incomp, comp, paired = TRUE)
  expect_equal(tValueString(tObj), "\\\\emph{t}(99) = 1.70")

  tObj   <- t.test(incomp, comp, paired = FALSE)
  expect_equal(tValueString(tObj), "\\\\emph{t}(193.59) = 1.83")

})
