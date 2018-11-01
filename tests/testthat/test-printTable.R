context("printTable")

test_that("printTable", {

  requiredPackages(c("ez"))

  # create dataframe
  dat <- createDF(nVP = 6, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = list(list(c("Comp:comp"), vals = c(500, 150, 100)),
                                  list(c("Comp:incomp"), vals = c(520, 150, 100))))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)

  expect_error(printTable(dat), NA)
  expect_error(printTable(aovRT$ANOVA), NA)

})
