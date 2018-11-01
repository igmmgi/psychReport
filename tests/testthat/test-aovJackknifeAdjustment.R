context("aovJackknifeAdjustment")

test_that("aovJackknifeAdjustment", {

  set.seed(1)

  # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
  dat <- createDF(nVP = 50, nTrl = 1,
                  design = list("Comp" = c("comp", "incomp")))

  dat <- addDataDF(dat,
                   RT = list(list(c("Comp:comp"), vals = c(500, 30, 50)),
                             list(c("Comp:incomp"), vals = c(800, 30, 50))))

  # repeated measures ANOVA using ezANOVA
  aovRT <- ezANOVA(dat, dv = .(RT), wid = .(VP), within = .(Comp),
                       return_aov = TRUE, detailed = TRUE)
  aovRT_pre  <- aovTable(aovRT)
  aovRT      <- aovJackknifeAdjustment(aovRT, length(unique(dat$VP)))
  aovRT_post <- aovTable(aovRT)

  expect_equal(round(as.numeric(aovRT_pre$ANOVA$F)/(49*49), 2), as.numeric(aovRT_post$ANOVA$F))

})
