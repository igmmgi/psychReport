context("meanStrAov")

test_that("meanStrAov", {

  set.seed(1)

  # simulated data for ANOVA
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = list(list(c("Comp:comp"), vals = c(500, 150, 100)),
                                  list(c("Comp:incomp"), vals = c(500, 150, 100))))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  string <- meanStrAov(aovRT, "Comp", "comp", unit = "ms")
  expect_equal(string, "523 ms")

  string <- meanStrAov(aovRT, "Comp", "comp", unit = "%")
  expect_equal(string, "523 \\\\%")

  # simulated data for ANOVA
  dat <- createDF(nVP = 20, nTrl = 1, design = list("Comp" = c("comp", "incomp", "neutral"),
                                                     "Side" = c("left", "right")))
  dat <- addDataDF(dat)

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp, Side),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  string <- meanStrAov(aovRT, "Comp:Side", "comp:left")
  expect_equal(string, "659 ms")

})
