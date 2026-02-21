# Extracted from test-aovEffectSize.R:21

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "psychReport", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("aovEffectSize")

# test -------------------------------------------------------------------------
set.seed(1)
dat <- createDF(nVP = 20, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))
dat <- addDataDF(dat, RT = list("Comp comp"   = c(500, 150, 100),
                                  "Comp incomp" = c(550, 150, 100)))
aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
testthat::expect_error(aovEffectSize(aovRT, effectSize = "pes"), NA)
testthat::expect_error(aovEffectSize(aovRT, effectSize = "ges"), NA)
aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                   return_aov = TRUE, detailed = TRUE)
