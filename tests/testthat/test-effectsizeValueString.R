context("statStrT")

test_that("statStrT", {

  set.seed(1)

  dat <- createDF(nVP = 20, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))

  dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 150, 100),
                                  "Comp_incomp" = c(550, 150, 100)))

  dat$VP <- as.factor(dat$VP)

  # partial eta-squared
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  effectString <- effectsizeValueString(aovRT, effect = "Comp")
  testthat::expect_equal(effectString, "$\\eta_{p}^2$ = 0.05")

  # unknown effect size raises error
  testthat::expect_error(effectsizeValueString(aovRT, effect = "Comp", effectSize = "zzz"))

})
