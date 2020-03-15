context("printAovMeans")

test_that("printAovMeans", {

  # create dataframe
  dat <- createDF(nVP = 6, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 150, 100),
                                  "Comp_incomp" = c(520, 150, 100)))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)
  expect_error(printAovMeans(aovRT), NA)

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = FALSE, detailed = TRUE)
  expect_error(printAovMeans(aovRT))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
  aovRT <- aovTable(aovRT)
  expect_error(printAovMeans(aovRT, digits = c(2, 2)))
  expect_error(printAovMeans(aovRT, dv = c("ms1", "ms2")))

})
