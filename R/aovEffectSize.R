#' @title aovEffectSize
#'
#' @description Add partial eta squared (pes) or eta-squared (es) effect size measures
#' to ezANOVA table.
#'
#' @param ezObj Output from ezANOVA
#' @param effectSize "ges" vs. pes" vs "es"
#'
#' @return list
#'
#' @examples
#' library(psychReport)
#' requiredPackages(c("ez", "dplyr"))
#' # Example 1:
#' # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list(list(c("Comp:comp", "Side:left"), vals = c(500, 150, 150)),
#'                            list(c("Comp:comp", "Side:right"), vals = c(500, 150, 150)),
#'                            list(c("Comp:incomp", "Side:left"), vals = c(500, 150, 150)),
#'                            list(c("Comp:incomp", "Side:right"), vals = c(500, 150, 150))))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovEffectSize(aovRT, "ges")
#' aovRT <- aovEffectSize(aovRT, "pes")
#' aovRT <- aovEffectSize(aovRT, "es")
#' aovDispTable(aovRT)
#'
#' @export
aovEffectSize <- function(ezObj, effectSize) {

  if (effectSize == "ges") {
    ezObj$ANOVA$ges <- ezObj$ANOVA$SSn / (ezObj$ANOVA$SSn + sum(ezObj$ANOVA$SSd))
    ezObj$ANOVA$es  <- NULL
    ezObj$ANOVA$pes <- NULL
  } else if (effectSize == "pes") {
    ezObj$ANOVA$ges <- NULL
    ezObj$ANOVA$es  <- NULL
    ezObj$ANOVA$pes <- ezObj$ANOVA$SSn / (ezObj$ANOVA$SSn + ezObj$ANOVA$SSd)
  } else if (effectSize == "es") {
    ezObj$ANOVA$ges <- NULL
    ezObj$ANOVA$pes <- NULL
    intercept       <- which(ezObj$ANOVA$Effect == "(Intercept)")
    effects         <- which(ezObj$ANOVA$Effect != "(Intercept)")
    ezObj$ANOVA$es  <- ezObj$ANOVA$SSn / (sum(ezObj$ANOVA$SSn[effects]) + sum(ezObj$ANOVA$SSd[effects]) + ezObj$ANOVA$SSd[intercept])
  } else {
    stop("effectSize not recognized!")
  }

  return(ezObj)

}
