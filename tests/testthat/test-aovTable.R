context("aovTable")

test_that("aovTable", {

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

  testthat::expect_error(aovTable(aovRT), NA)
  testthat::expect_error(aovTable(aovRT, sphericityCorrections = FALSE), NA)
  testthat::expect_error(aovTable(aovRT, sphericityCorrectionType = "HF"), NA)
  testthat::expect_error(aovTable(aovRT, removeSumSquares = FALSE), NA)
  testthat::expect_error(aovTable(aovRT, removeIntercept = FALSE), NA)
  testthat::expect_error(aovTable(aovRT, marginalMeans = FALSE), NA)
  testthat::expect_error(aovTable(aovRT, roundDigits = FALSE), NA)
  testthat::expect_error(aovTable(aovRT, numDigits = 4), NA)
  testthat::expect_error(aovTable(aovRT, dispAovTable = FALSE), NA)
  testthat::expect_error(aovTable(aovRT, dispAovMeans = TRUE), NA)

  aovRT <- aovTable(aovRT)
  testthat::expect_error(aovTable(aovRT))

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                       return_aov = TRUE, detailed = FALSE)
  testthat::expect_error(aovTable(aovRT))

  aovRT <- ez::ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                       return_aov = FALSE, detailed = TRUE)
  testthat::expect_error(aovTable(aovRT))

})

