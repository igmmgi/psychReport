#' @title aovEffectSize
#'
#' @description Add partial eta squared (pes) or eta-squared (es) effect size measures
#' to ezANOVA table.
#'
#' @param ezObj Output from ezANOVA
#' @param effectSize "ges" vs. pes"
#'
#' @return list
#'
#' @examples
#' requiredPackages(c("ez", "dplyr"))
#'
#' # Example 1:
#' # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp", "neutral"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp:Side_comp:left"     = c(500, 150, 150),
#'                            "Comp:Side_comp:right"    = c(500, 150, 150),
#'                            "Comp:Side_incomp:left"   = c(550, 150, 150),
#'                            "Comp:Side_incomp:right"  = c(550, 150, 150),
#'                            "Comp:Side_neutral:left"  = c(525, 150, 150),
#'                            "Comp:Side_neutral:right" = c(525, 150, 150)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side), 
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovEffectSize(aovRT, "pes")
#' aovRT <- aovTable(aovRT)
#'
#' @export
aovEffectSize <- function(ezObj, effectSize) {

   if (effectSize == "ges") {
    # NB assumes no observed variables within initial call to ezANOVA!
    ezObj$ANOVA$ges <- ezObj$ANOVA$SSn / (ezObj$ANOVA$SSn + sum(unique(ezObj$ANOVA$SSd)))
    ezObj$ANOVA$pes <- NULL
  } else if (effectSize == "pes") {
    ezObj$ANOVA$ges <- NULL
    ezObj$ANOVA$pes <- ezObj$ANOVA$SSn / (ezObj$ANOVA$SSn + ezObj$ANOVA$SSd)
  } else {
    stop("effectSize not recognized!")
  }

  return(ezObj)

}
