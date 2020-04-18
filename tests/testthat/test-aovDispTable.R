context("aovDispTable")

test_that("aovDispTable", {

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
  testthat::expect_error(aovDispTable(aovRT), NA)

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = FALSE, detailed = TRUE)
  testthat::expect_error(aovDispTable(aovRT), NA)

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = FALSE, detailed = FALSE)
  testthat::expect_error(aovDispTable(aovRT), NA)

})

