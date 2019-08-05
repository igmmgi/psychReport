#' @title effectsizeValueString
#'
#' @description Returns required Latex formatted string for effect size (eta squared, partial
#' eta square, generalized eta squared) = XXX for R/Sweave integration.
#  Returns values to 2 sig decimal places.

#' @param ezObj The returned object from a call to ezANOVA
#' @param effect The effect within the ANOVA table to return
#' @param effectSize The effect size to report ("ges" default witin ezANOVA, "pes" partial eta squared)
#'
#' @return character
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat, RT = list(list(c("Comp:comp", "Side:left"), vals = c(500, 150, 100)),
#'                                 list(c("Comp:comp", "Side:right"), vals = c(500, 150, 100)),
#'                                 list(c("Comp:incomp", "Side:left"), vals = c(520, 150, 100)),
#'                                 list(c("Comp:incomp", "Side:right"), vals = c(520, 150, 100))))
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
#' aovRT <- aovTable(aovRT)
#'
#' pesString <- effectsizeValueString(aovRT, "Comp")  # partial eta squared
#' pesString <- effectsizeValueString(aovRT, "Comp:Side")
#'
#' \dontrun{
#' # Example use in *.Rnw Sweave file
#' # \Sexpr{pesString}}
#'
#' @export

effectsizeValueString <- function(ezObj, effect, effectSize = "pes"){

  effectSizeIdx <- which(names(ezObj$ANOVA) %in% effectSize)
  if (length(effectSizeIdx) == 0) {
    stop("Requested effect size not present in ANOVA table!")
  }
  if (effectSize == "ges") {
    effectSizeValue <- ezObj$ANOVA[, "ges"][ezObj$ANOVA$Effect == effect]
    return(paste0("$\\eta_{G}^2$ = ", effectSizeValue))
  } else if (effectSize == "pes") {
    effectSizeValue  <- ezObj$ANOVA[, "pes"][ezObj$ANOVA$Effect == effect]
    return(paste0("$\\eta_{p}^2$ = ", effectSizeValue))
  }
}
