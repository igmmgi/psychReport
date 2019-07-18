context("statStrT")

test_that("statStrT", {

  set.seed(1)

  dat <- createDF(nVP = 20, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))

  dat <- addDataDF(dat, RT = list(list(c("Comp:comp"), vals = c(500, 150, 100)),
                                  list(c("Comp:incomp"), vals = c(550, 150, 100))))

  # generalized eta-squared
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT, effectSize = "ges")

  effectString <- effectsizeValueString(aovRT, effect = "Comp", effectSize = "ges")
  expect_equal(effectString, "$\\eta_{G}^2$ = 0.03")

  # partial eta-squared
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT, effectSize = "pes")

  effectString <- effectsizeValueString(aovRT, effect = "Comp", effectSize = "pes")
  expect_equal(effectString, "$\\eta_{p}^2$ = 0.05")

  # eta-squared
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT, effectSize = "es")

  effectString <- effectsizeValueString(aovRT, effect = "Comp", effectSize = "es")
  expect_equal(effectString, "$\\eta^2$ = 0.03")

  # unknow effect size raises error
  expect_error(effectsizeValueString(aovRT, effect = "Comp", effectSize = "zzz"))

})
