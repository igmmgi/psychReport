context("statStrAov")

test_that("statStrAov", {

  set.seed(1)

  # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp"),
                                "Side" = c("left", "right")))

  dat <- addDataDF(dat, RT = list(list(c("Comp:comp", "Side:left"), vals = c(500, 150, 100)),
                                  list(c("Comp:comp", "Side:right"), vals = c(500, 150, 100)),
                                  list(c("Comp:incomp", "Side:left"), vals = c(550, 150, 100)),
                                  list(c("Comp:incomp", "Side:right"), vals = c(550, 150, 100))))

  # repeated measures ANOVA using ezANOVA
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp, Side),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  aovStringComp     <- statStrAov(aovRT, "Comp")
  aovStringSide     <- statStrAov(aovRT, "Side")
  aovStringCompSide <- statStrAov(aovRT, "Comp:Side")

  expect_equal(aovStringComp,     "\\emph{F}(1, 49) = 4.37, \\emph{p} = .04, $\\eta_{p}^2$ = 0.08")
  expect_equal(aovStringSide,     "\\emph{F}(1, 49) = 0.32, \\emph{p} = .57, $\\eta_{p}^2$ = 0.01")
  expect_equal(aovStringCompSide, "\\emph{F}(1, 49) = 0.49, \\emph{p} = .49, $\\eta_{p}^2$ = 0.01")

  # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "neutral", "incomp")))
  dat <- addDataDF(dat)

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  aovStringComp <- statStrAov(aovRT, "Comp")
  expect_equal(aovStringComp, "\\emph{F}(2, 98) = 0.02, \\emph{p} = .98, $\\eta_{p}^2$ = 0.00, $\\epsilon$ = 0.97")

})
