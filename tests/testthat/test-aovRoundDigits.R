context("aovRoundDigits")

test_that("aovRoundDigits", {

  set.seed(1)

  # create dataframe
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "neutral", "incomp")))

  dat <- addDataDF(dat,
                   RT = list("Comp_comp"    = c(500, 150, 150),
                             "Comp_neutral" = c(550, 150, 150),
                             "Comp_incomp"  = c(600, 150, 150)))

  # base R aov
  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  aovRT <- aovRoundDigits(aovRT, nsmall = 2)

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "2.17")

  aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
  aovRT <- aovRoundDigits(aovRT, 1)

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "2.20")

  # repeated measures ANOVA using ezANOVA
  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                       return_aov = TRUE, detailed = TRUE)
  aovRT <- aovRoundDigits(aovRT, 3)  # 3 sig decimal places

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "1346.037")
  testthat::expect_equal(as.character(aovRT$ANOVA$F[2]), "2.170")

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                       return_aov = TRUE, detailed = TRUE)
  aovRT <- aovRoundDigits(aovRT, 1)  # 1 sig decimal places

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "1346.00")
  testthat::expect_equal(as.character(aovRT$ANOVA$F[2]), "2.20")

})
