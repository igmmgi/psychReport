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

  dat$VP <- as.factor(dat$VP)

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovRoundDigits(aovRT, 3)  # 3 sig decimal places

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "1346.767")
  testthat::expect_equal(as.character(aovRT$ANOVA$F[2]), "   2.171")

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovRoundDigits(aovRT, 1)  # 1 sig decimal places

  testthat::expect_equal(as.character(aovRT$ANOVA$F[1]), "1346.8")
  testthat::expect_equal(as.character(aovRT$ANOVA$F[2]), "   2.2")

})
