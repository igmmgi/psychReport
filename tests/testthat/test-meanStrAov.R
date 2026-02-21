context("meanStrAov")

test_that("meanStrAov", {

  set.seed(1)

  # simulated data for ANOVA
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 150, 100),
                                  "Comp incomp" = c(500, 150, 100)))

  # base R aov
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)

  string <- meanStrAov(aovRT, "Comp", "comp", unit = "ms")
  testthat::expect_equal(string, "523 ms")

  string <- meanStrAov(aovRT, "Comp", "comp", unit = "%")
  testthat::expect_equal(string, "523 \\%")

  # simulated data for multi-factor ANOVA
  dat <- createDF(nVP = 20, nTrl = 1, design = list("Comp" = c("comp", "incomp", "neutral"),
                                                     "Side" = c("left", "right")))
  dat <- addDataDF(dat)

  dat$VP <- as.factor(dat$VP)

  aovRT <- aov(RT ~ Comp*Side + Error(VP/(Comp*Side)), dat)
  aovRT <- aovTable(aovRT, sphericityCorrections = FALSE)

  string <- meanStrAov(aovRT, "Comp:Side", "comp:left")
  testthat::expect_equal(string, "658 ms")

})

