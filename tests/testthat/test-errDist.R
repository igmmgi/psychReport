context("errDist")

test_that("errDist", {

  # test 1: 10% errors
  set.seed(1)
  errs <- errDist(1000, 10)

  expect_equal(length(errs), 1000)
  expect_equal(sum(errs), 96)

  # test 2: 20% errors
  set.seed(1)
  errs <- errDist(2000, 20)

  expect_equal(length(errs), 2000)
  expect_equal(sum(errs), 428)

  # test 3: 30% errors
  set.seed(1)
  errs <- errDist(3000, 30)

  expect_equal(length(errs), 3000)
  expect_equal(sum(errs), 930)

  # test 5: 100% errors
  set.seed(1)
  errs <- errDist(10, 100)

  expect_equal(length(errs), 10)
  expect_equal(sum(errs), 10)

})
