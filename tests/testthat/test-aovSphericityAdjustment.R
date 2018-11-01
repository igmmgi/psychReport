context("aovSphericityAdjustment")

test_that("aovSphericityAdjustment", {

  set.seed(1)

  # create dataframe
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "neutral", "incomp")))

  dat <- addDataDF(dat,
                   RT = list(list(c("Comp:comp"),    vals = c(500, 150, 150)),
                             list(c("Comp:neutral"), vals = c(550, 150, 150)),
                             list(c("Comp:incomp"),  vals = c(600, 150, 150))))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)

  expect_error(aovTable(aovRT, sphericityCorrectionType = "HF"), NA)
  expect_error(aovTable(aovRT, sphericityCorrectionType = "GG"), NA)
  expect_error(aovTable(aovRT, sphericityCorrectionType = "HG"))

})
