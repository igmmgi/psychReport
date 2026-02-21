context("aovTable")

test_that("aovTable", {

  set.seed(1)

  # create dataframe with 3 levels
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "neutral", "incomp")))

  dat <- addDataDF(dat,
                   RT = list("Comp comp"    = c(500, 150, 150),
                             "Comp neutral" = c(550, 150, 150),
                             "Comp incomp"  = c(600, 150, 150)))

  # base R aov with sphericity corrections (default)
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTable(aovRT), NA)

  # without sphericity corrections
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTable(aovRT, sphericityCorrections = FALSE), NA)

  # HF correction
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTable(aovRT, sphericityCorrectionType = "HF"), NA)

  # keep sum of squares
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTable(aovRT, removeSumSquares = FALSE), NA)

  # calling aovTable twice should error (already processed)
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  aovRT <- aovTable(aovRT)
  testthat::expect_error(aovTable(aovRT))

})

