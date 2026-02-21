context("aovSphericityAdjustment")

test_that("aovSphericityAdjustment", {

  set.seed(1)

  # create dataframe with 3 levels (sphericity is relevant for 3+ levels)
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "neutral", "incomp")))

  dat <- addDataDF(dat,
                   RT = list("Comp comp"    = c(500, 150, 150),
                             "Comp neutral" = c(550, 150, 150),
                             "Comp incomp"  = c(600, 150, 150)))

  # base R aov with sphericity corrections (GG)
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTable(aovRT, sphericityCorrectionType = "GG"), NA)

  # HF correction
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTable(aovRT, sphericityCorrectionType = "HF"), NA)

  # invalid correction type should error
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTable(aovRT, sphericityCorrectionType = "HG"))

  # without sphericity corrections should also work
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  testthat::expect_error(aovTable(aovRT, sphericityCorrections = FALSE), NA)

  # 2-level factor should work fine (no sphericity correction needed)
  dat2 <- createDF(nVP = 20, nTrl = 1,
                   design = list("Comp" = c("comp", "incomp")))
  dat2 <- addDataDF(dat2,
                    RT = list("Comp comp"   = c(500, 150, 150),
                              "Comp incomp" = c(550, 150, 150)))
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat2)
  testthat::expect_error(aovTable(aovRT), NA)

})


test_that("adjDF with significant Mauchly's test", {

  # Construct data that violates sphericity:
  # Large variance in one condition, small in others
  set.seed(42)
  n <- 40
  dat <- data.frame(
    VP   = factor(rep(1:n, each = 3)),
    Comp = factor(rep(c("A", "B", "C"), n)),
    RT   = 0
  )
  # Create non-spherical data: condition C highly correlated with A, B independent
  subj_effect <- rnorm(n, 0, 100)
  dat$RT[dat$Comp == "A"] <- 500 + subj_effect + rnorm(n, 0, 10)
  dat$RT[dat$Comp == "B"] <- 500 + rnorm(n, 0, 80)
  dat$RT[dat$Comp == "C"] <- 500 + subj_effect * 2 + rnorm(n, 0, 10)

  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  aovData <- stats::model.frame(aovRT)
  result <- aovTidyTable(aovRT, data = aovData)

  # Verify Mauchly's test is significant
  testthat::expect_true(result$"Mauchly's Test for Sphericity"$p < 0.05)

  # adjDF = TRUE should adjust DFs
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  result_adj <- aovTable(aovRT, sphericityCorrectionAdjDF = TRUE,
                          removeSumSquares = FALSE)

  # DFn should be adjusted (original is 2, adjusted should be < 2)
  DFn <- as.numeric(result_adj$ANOVA$DFn[result_adj$ANOVA$Effect == "Comp"])
  testthat::expect_true(DFn < 2)

  # DFd should be adjusted too
  DFd <- as.numeric(result_adj$ANOVA$DFd[result_adj$ANOVA$Effect == "Comp"])
  testthat::expect_true(DFd < 39 * 2)  # original DFd = (n-1)*2 = 78

  # adjDF = FALSE should NOT adjust DFs
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  result_noadj <- aovTable(aovRT, sphericityCorrectionAdjDF = FALSE)
  DFn_noadj <- as.numeric(result_noadj$ANOVA$DFn[result_noadj$ANOVA$Effect == "Comp"])
  testthat::expect_equal(DFn_noadj, 2)

  # message when adjDF = TRUE but Mauchly not significant
  dat_sph <- createDF(nVP = 50, nTrl = 1,
                      design = list("Comp" = c("comp", "neutral", "incomp")))
  dat_sph <- addDataDF(dat_sph,
                       RT = list("Comp comp"    = c(500, 150, 150),
                                 "Comp neutral" = c(550, 150, 150),
                                 "Comp incomp"  = c(600, 150, 150)))
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat_sph)
  testthat::expect_message(
    aovTable(aovRT, sphericityCorrectionAdjDF = TRUE),
    "Mauchly's test not significant"
  )

})

