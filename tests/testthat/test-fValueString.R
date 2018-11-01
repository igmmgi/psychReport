context("fValueString")

test_that("fValueString", {

  # simulated data for ANOVA
  set.seed(1)
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp"),
                                "Side" = c("left", "right")))
  dat <- addDataDF(dat, RT = list(list(c("Comp:comp", "Side:left"), vals = c(500, 150, 100)),
                                  list(c("Comp:comp", "Side:right"), vals = c(500, 150, 100)),
                                  list(c("Comp:incomp", "Side:left"), vals = c(550, 150, 100)),
                                  list(c("Comp:incomp", "Side:right"), vals = c(550, 150, 100))))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp, Side),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  expect_equal(fValueString(aovRT, "Comp"), "\\\\emph{F}(1, 49) = 4.37")
  expect_equal(fValueString(aovRT, "Side"), "\\\\emph{F}(1, 49) = 0.32")
  expect_equal(fValueString(aovRT, "Comp:Side"), "\\\\emph{F}(1, 49) = 0.49")

})
