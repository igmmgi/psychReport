context("sphericityValueString")

test_that("sphericityValueString", {

  set.seed(1)

  # create dataframe with 3 levels (sphericity relevant)
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("neutral", "comp", "incomp")))
  dat <- addDataDF(dat, RT = list("Comp neutral" = c(510, 150, 100),
                                  "Comp comp"    = c(500, 150, 100),
                                  "Comp incomp"  = c(520, 150, 100)))

  # base R aov with GG correction
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  aovRT <- aovTable(aovRT, sphericityCorrectionType = "GG")

  sphericityValue <- sphericityValueString(aovRT, "Comp")
  testthat::expect_true(!is.null(sphericityValue))
  testthat::expect_true(grepl("epsilon", sphericityValue))

  # HF correction
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  aovRT <- aovTable(aovRT, sphericityCorrectionType = "HF")

  sphericityValue <- sphericityValueString(aovRT, "Comp")
  testthat::expect_true(!is.null(sphericityValue))

  # 2-level factor should return NULL (no sphericity info)
  dat2 <- createDF(nVP = 50, nTrl = 1,
                   design = list("Comp" = c("comp", "incomp")))
  dat2 <- addDataDF(dat2, RT = list("Comp comp"   = c(500, 150, 100),
                                    "Comp incomp" = c(520, 150, 100)))

  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat2)
  aovRT <- aovTable(aovRT)

  sphericityValue <- sphericityValueString(aovRT, "Comp")
  testthat::expect_null(sphericityValue)

})

