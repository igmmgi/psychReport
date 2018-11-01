context("aovEffectSize")

test_that("aovEffectSize", {

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

  # generalized eta squared
  aovRT <- aovEffectSize(aovRT, effectSize = "ges")

  expect_equal(aovRT$ANOVA$ges[2], 0.03038679)
  expect_equal(aovRT$ANOVA$pes[2], NULL)
  expect_equal(aovRT$ANOVA$es[2], NULL)

  # partial eta squared
  aovRT <- aovEffectSize(aovRT, effectSize = "pes")

  expect_equal(aovRT$ANOVA$pes[2], 0.04243096)
  expect_equal(aovRT$ANOVA$ges[2], NULL)
  expect_equal(aovRT$ANOVA$es[2], NULL)

  # eta squared
  aovRT <- aovEffectSize(aovRT, effectSize = "es")

  expect_equal(aovRT$ANOVA$es[2], 0.03038679)
  expect_equal(aovRT$ANOVA$ges[2], NULL)
  expect_equal(aovRT$ANOVA$pes[2], NULL)

  # non effect size
  expect_error(aovEffectSize(aovRT, effectSize = "abc"))

})

