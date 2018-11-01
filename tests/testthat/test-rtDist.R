context("rtDist")

test_that("rtDist", {

  # test 1
  set.seed(1)
  x <- rtDist()
  expect_equal(length(x), 10000)
  expect_equal(600, round(mean(x)))

  # test 2
  set.seed(1)
  x <- rtDist(n = 100000, gaussMean = 500)
  expect_equal(length(x), 100000)
  expect_equal(500, round(mean(x)))

})
