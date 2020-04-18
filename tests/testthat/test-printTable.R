context("printTable")

test_that("printTable", {

  # create dataframe
  dat <- createDF(nVP = 6, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 150, 100),
                                  "Comp_incomp" = c(520, 150, 100)))

  dat$VP <- as.factor(dat$VP)

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  testthat::expect_error(printTable(dat), NA)
  testthat::expect_error(printTable(aovRT$ANOVA), NA)

})
