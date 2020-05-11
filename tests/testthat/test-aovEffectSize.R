context("aovEffectSize")

test_that("aovEffectSize", {

  set.seed(1)

  # create dataframe
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "neutral", "incomp")))

  dat <- addDataDF(dat,
                   RT = list("Comp_comp"    = c(500, 150, 150),
                             "Comp_neutral" = c(550, 150, 150),
                             "Comp_incomp"  = c(600, 150, 150)))

  dat$VP <- as.factor(dat$VP)

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)

  # partial eta squared
  aovRT <- aovEffectSize(aovRT)

  testthat::expect_equal(aovRT$ANOVA$pes[2], 0.04240754)

  # non effect size
  testthat::expect_error(aovEffectSize(aovRT, effectSize = "abc"))

})

