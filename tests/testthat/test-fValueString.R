context("fValueString")

test_that("fValueString", {

  # simulated data for ANOVA
  set.seed(1)
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp"),
                                "Side" = c("left", "right")))
  dat <- addDataDF(dat, RT = list("Comp:Side_comp:left"    = c(500, 150, 100),
                                  "Comp:Side_comp:right"   = c(500, 150, 100),
                                  "Comp:Side_incomp:left"  = c(550, 150, 100),
                                  "Comp:Side_incomp:right" = c(550, 150, 100)))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp, Side),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  expect_equal(fValueString(aovRT, "Comp"), "\\emph{F}(1, 49) = 4.37")
  expect_equal(fValueString(aovRT, "Side"), "\\emph{F}(1, 49) = 0.32")
  expect_equal(fValueString(aovRT, "Comp:Side"), "\\emph{F}(1, 49) = 0.49")

})
