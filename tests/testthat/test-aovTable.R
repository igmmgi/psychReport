context("aovTable")

test_that("aovTable", {

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

  expect_error(aovTable(aovRT), NA)
  expect_error(aovTable(aovRT, effectSize = "es"), NA)
  expect_error(aovTable(aovRT, sphericityCorrections = FALSE), NA)
  expect_error(aovTable(aovRT, sphericityCorrectionType = "HF"), NA)
  expect_error(aovTable(aovRT, removeSumSquares = FALSE), NA)
  expect_error(aovTable(aovRT, removeIntercept = FALSE), NA)
  expect_error(aovTable(aovRT, marginalMeans = FALSE), NA)
  expect_error(aovTable(aovRT, roundDigits = FALSE), NA)
  expect_error(aovTable(aovRT, numDigits = 4), NA)
  expect_error(aovTable(aovRT, dispAovTable = FALSE), NA)
  expect_error(aovTable(aovRT, dispAovMeans = TRUE), NA)

  aovRT <- aovTable(aovRT)
  expect_error(aovTable(aovRT))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = FALSE)
  expect_error(aovTable(aovRT))

  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = FALSE, detailed = TRUE)
  expect_error(aovTable(aovRT))

})

