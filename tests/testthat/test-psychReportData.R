context("psychReportData")

test_that("createDF", {

  # default
  dat <- createDF()
  testthat::expect_true(is.data.frame(dat))
  testthat::expect_equal(nrow(dat), 20 * 50 * 2 * 2)
  testthat::expect_true("VP" %in% names(dat))

  # custom design
  dat <- createDF(nVP = 10, nTrl = 5,
                  design = list("Comp" = c("comp", "incomp", "neutral")))
  testthat::expect_equal(nrow(dat), 10 * 5 * 3)
  testthat::expect_true(all(c("VP", "Comp") %in% names(dat)))

})


test_that("addDataDF", {

  set.seed(1)

  # default (no RT/Error params)
  dat <- createDF(nVP = 10, nTrl = 5,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat)
  testthat::expect_true("RT" %in% names(dat))
  testthat::expect_true("Error" %in% names(dat))
  testthat::expect_true(all(dat$Error %in% c(0, 1)))

  # with scalar RT params
  dat <- createDF(nVP = 10, nTrl = 5,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, RT = c(500, 100, 100))
  testthat::expect_true(all(dat$RT > 0))

  # with list RT params (per-condition)
  dat <- createDF(nVP = 10, nTrl = 5,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp comp"   = c(400, 50, 50),
                             "Comp incomp" = c(500, 50, 50)))
  testthat::expect_true(all(dat$RT > 0))

  # with scalar Error params
  dat <- createDF(nVP = 10, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat, Error = 20)
  testthat::expect_true(sum(dat$Error) > 0)

  # with list Error params (per-condition with bins)
  dat <- createDF(nVP = 10, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp comp"   = c(400, 50, 50),
                             "Comp incomp" = c(500, 50, 50)),
                   Error = list("Comp comp"   = c(5, 10),
                                "Comp incomp" = c(10, 15)))
  testthat::expect_true(sum(dat$Error) > 0)
  testthat::expect_false("bins" %in% names(dat))

  # with multi-factor interaction
  dat <- createDF(nVP = 10, nTrl = 5,
                  design = list("Comp" = c("comp", "incomp"),
                                "Side" = c("left", "right")))
  dat <- addDataDF(dat,
                   RT = list("Comp:Side comp:left"    = c(400, 50, 50),
                             "Comp:Side comp:right"   = c(420, 50, 50),
                             "Comp:Side incomp:left"  = c(500, 50, 50),
                             "Comp:Side incomp:right" = c(520, 50, 50)))
  testthat::expect_true(all(dat$RT > 0))

})


test_that("rtDist", {

  set.seed(1)

  x <- rtDist()
  testthat::expect_equal(length(x), 10000)
  testthat::expect_true(all(is.numeric(x)))

  x <- rtDist(n = 100, gaussMean = 500, gaussSD = 50, expRate = 100)
  testthat::expect_equal(length(x), 100)

})


test_that("errDist", {

  set.seed(1)

  x <- errDist(1000)
  testthat::expect_equal(length(x), 1000)
  testthat::expect_true(all(x %in% c(0, 1)))
  # ~10% errors by default
  testthat::expect_true(mean(x) > 0.05 && mean(x) < 0.20)

  x <- errDist(1000, 50)
  # ~50% errors
  testthat::expect_true(mean(x) > 0.35 && mean(x) < 0.65)

})


test_that("summaryMSDSE", {

  set.seed(1)

  dat <- createDF(nVP = 20, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp comp"   = c(500, 80, 100),
                             "Comp incomp" = c(550, 80, 140)),
                   Error = list("Comp comp"   = 5,
                                "Comp incomp" = 10))

  datAggVP <- dat %>%
    dplyr::group_by(VP, Comp) %>%
    dplyr::summarize(
      N  = dplyr::n(),
      RT = mean(RT[Error == 0]),
      ER = (sum(Error) / N) * 100,
      .groups = "drop"
    )

  # basic summary
  datAgg <- summaryMSDSE(datAggVP, "Comp", c("RT", "ER"))
  testthat::expect_true(is.data.frame(datAgg))
  testthat::expect_true("RT_mean" %in% names(datAgg))
  testthat::expect_true("RT_sd" %in% names(datAgg))
  testthat::expect_true("RT_se" %in% names(datAgg))
  testthat::expect_true("RT_se_ci" %in% names(datAgg))
  testthat::expect_true("ER_mean" %in% names(datAgg))
  testthat::expect_equal(nrow(datAgg), 2)  # comp, incomp

  # with within-subjects correction
  datAggVP <- normData(datAggVP, "VP", c("RT", "ER"))
  datAgg <- summaryMSDSE(datAggVP, "Comp", c("RT", "ER", "RT_norm", "ER_norm"),
                          c("RT_norm", "ER_norm"))
  testthat::expect_true("RT_norm_sd" %in% names(datAgg))
  testthat::expect_true("RT_norm_se" %in% names(datAgg))

})


test_that("normData", {

  set.seed(1)

  dat <- createDF(nVP = 20, nTrl = 50,
                  design = list("Comp" = c("comp", "incomp")))
  dat <- addDataDF(dat,
                   RT = list("Comp comp"   = c(500, 80, 100),
                             "Comp incomp" = c(550, 80, 100)))

  datAggVP <- dat %>%
    dplyr::group_by(VP, Comp) %>%
    dplyr::summarize(
      RT = mean(RT),
      .groups = "drop"
    )

  datNorm <- normData(datAggVP, "VP", "RT")
  testthat::expect_true("RT_norm" %in% names(datNorm))

  # Grand mean of norm column should equal grand mean of original
  testthat::expect_equal(mean(datNorm$RT_norm), mean(datNorm$RT), tolerance = 0.01)

  # Multiple DVs
  datAggVP$ER <- runif(nrow(datAggVP), 5, 15)
  datNorm <- normData(datAggVP, "VP", c("RT", "ER"))
  testthat::expect_true("RT_norm" %in% names(datNorm))
  testthat::expect_true("ER_norm" %in% names(datNorm))

})

