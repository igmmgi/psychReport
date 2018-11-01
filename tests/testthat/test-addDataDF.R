context("addDataDF")

test_that("addDataDF", {

  # default 2*2
  dat <- createDF(nTrl = 100)
  dat <- addDataDF(dat)

  expect_equal(nrow(dat), 8000)
  expect_equal(ncol(dat), 5)
  expect_equal(names(dat), c("VP", "A", "B", "RT", "Error"))

  # default 2*2 with defined rt + error rate
  dat <- createDF(nTrl = 100)
  dat <- addDataDF(dat, RT = c(500, 150, 100), Error = c(10))

  expect_equal(nrow(dat), 8000)
  expect_equal(ncol(dat), 5)
  expect_equal(names(dat), c("VP", "A", "B", "RT", "Error"))

  # 1 factor with 2 levels + defined rt and error rates
  dat <- createDF(nVP = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = list(list(c("Comp:comp"), vals = c(500, 150, 150)),
                                  list(c("Comp:incomp"), vals = c(550, 150, 150))),
                        Error = list(list(c("Comp:comp"), vals = c(10, 8, 6, 4)),
                                  list(c("Comp:incomp"), vals = c(15, 10, 7, 5))))

  expect_equal(nrow(dat), 20*50*2)
  expect_equal(ncol(dat), 4)
  expect_equal(names(dat), c("VP", "Comp", "RT", "Error"))

  # 1 factor with 3 levels
  dat <- createDF(nVP = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral")))
  dat <- addDataDF(dat)

  expect_equal(nrow(dat), 20*50*3)
  expect_equal(ncol(dat), 4)
  expect_equal(names(dat), c("VP", "Comp", "RT", "Error"))

  # 2 factors, 3 & 2 levels
  dat <- createDF(nVP = 20, nTrl = 50, design = list("Comp" = c("comp", "incomp", "neutral"),
                                                     "Side" = c("left", "right")))
  dat <- addDataDF(dat)

  expect_equal(nrow(dat), 20*50*6)
  expect_equal(ncol(dat), 5)
  expect_equal(names(dat), c("VP", "Comp", "Side", "RT", "Error"))

  # 1 factor, 1 VP, 1 trial
  dat <- createDF(nVP = 1, nTrl = 1, design = list("A" = c("a", "b")))
  dat <- addDataDF(dat)

  expect_equal(nrow(dat), 2)
  expect_equal(ncol(dat), 4)
  expect_equal(names(dat), c("VP", "A", "RT", "Error"))

})
