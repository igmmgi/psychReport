#' @title adjustJackknifeAdjustment
#'
#' @description Adjust ezANOVA table with corrected F (Fc = F/(n-1)^2) and p values for jackkniffed data (see Ulrich and Miller, 2001. Using the jackknife-based scoring method for measuring LRP onset effects in factorial designs. Psychophysiology, 38, 816-827.)
#'
#' @param ezObj Output from ezANOVA
#' @param numVPs The number of participants
#'
#' @return list
#'
#' @examples
#' library(psychReport)
#' requiredPackages(c("dplyr", "ez"))
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
#' aovDispTable(aovRT)
#' aovRT <- aovJackknifeAdjustment(aovRT, length(unique(datAggVP$VP)))
#' aovDispTable(aovRT)
#'
#' @export
aovJackknifeAdjustment <- function(ezObj, numVPs) {

  ezObj$ANOVA$SSd     <- ezObj$ANOVA$SSd*((numVPs - 1) ^ 2)
  ezObj$ANOVA$F       <- ezObj$ANOVA$F/((numVPs - 1) ^ 2)
  ezObj$ANOVA$p       <- 1 - stats::pf(ezObj$ANOVA$F, ezObj$ANOVA$DFn, ezObj$ANOVA$DFd)
  ezObj$ANOVA$"p<.05" <- pValueSummary(ezObj$ANOVA$p)

  return(ezObj)

}
