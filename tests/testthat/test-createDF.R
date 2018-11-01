context("createDF")

test_that("createDF", {

  # default
  dat <- createDF()
  expect_equal(nrow(dat), 4000)
  expect_equal(ncol(dat), 3)
  expect_equal(names(dat), c("VP", "A", "B"))

  # 1 factor, 2 levels
  dat <- createDF(nVP = 20, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  expect_equal(nrow(dat), 20*50*2)
  expect_equal(ncol(dat), 2)
  expect_equal(names(dat), c("VP", "Comp"))

  # 1 factor, 3 levels
  dat <- createDF(nVP = 15, nTrl = 25,
                  design = list("Comp" = c("neutral", "comp", "incomp")))
  expect_equal(nrow(dat), 15*25*3)
  expect_equal(ncol(dat), 2)
  expect_equal(names(dat), c("VP", "Comp"))

  # 2*2 factor, 2 levels each
  dat <- createDF(nVP = 10, nTrl = 25,
                  design = list("Comp" = c("comp", "incomp"),
                                "Side" = c("left", "right")))
  expect_equal(nrow(dat), 10*25*2*2)
  expect_equal(ncol(dat), 3)
  expect_equal(names(dat), c("VP", "Comp", "Side"))

})
