#' @title aovTable
#'
#' @description Adjust ezANOVA table output. Options include calculation of alternative
#' effect sizes (eta squared, partial eta squared), the calculation of marginal
#' means and formating options for the ANOVA table (e.g., detailed, rounding).
#'
#' @param ezObj Output from ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effectSize "ges" (generalized eta-squared), "es" (eta-squared), or
#' "pes" (partial eta-squared)
#' @param sphericityCorrections TRUE/FALSE
#' @param sphericityCorrectionType "GG" (default) vs. "HF"
#' @param marginalMeans Return marginal means via model.tables (NB. ezANOVA must
#' be called with return_aov = TRUE)
#' @param removeSumSquares TRUE/FALSE Remove SSn/SSd columns from the ANOVA table
#' @param removeIntercept TRUE/FALSE Remove intercept row from the ANOVA table
#' @param roundDigits TRUE/FALSE Round numerical values to numDigits
#' @param numDigits The number of digits to round to if roundDigits = TRUE
#' @param dispAovTable Display the ANOVA table in the command window
#' @param dispAovMeans Display the marginal means in the command window
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
#' aovRT <- aovTable(aovRT)
#'
#' @export
aovTable <- function(ezObj,
                     effectSize = "pes",
                     sphericityCorrections = TRUE,
                     sphericityCorrectionType = "GG",
                     marginalMeans = TRUE,
                     removeSumSquares = TRUE,
                     removeIntercept = TRUE,
                     roundDigits = TRUE,
                     numDigits = 2,
                     dispAovTable = TRUE,
                     dispAovMeans = FALSE) {

  if (!"SSn" %in% names(ezObj$ANOVA)) {
    stop("Call ezANOVA with \"detailed = TRUE\"!")
  }

  if (!"aov" %in% names(ezObj)) {
      stop("Call ezANOVA with \"return_aov = TRUE\"!")
  }

  if (effectSize != "ges") {
      ezObj <- aovEffectSize(ezObj, effectSize)
  }

  if (sphericityCorrections) {
    ezObj <- aovSphericityAdjustment(ezObj, sphericityCorrectionType)
  }

  if (removeIntercept) {
    ezObj$ANOVA <- ezObj$ANOVA[-c(1), ]
  }

  # p-value summary *** vs. ** vs *
  ezObj$ANOVA$"p<.05" <- pValueSummary(ezObj$ANOVA$p)

  if (roundDigits) {
    ezObj <- aovRoundDigits(ezObj, nsmall = numDigits)
  }

  if (removeSumSquares) {
    ezObj$ANOVA$SSn <- NULL
    ezObj$ANOVA$SSd <- NULL
  }

  if (marginalMeans) {
    ezObj$means <- stats::model.tables(ezObj$aov, type = "mean")
  }

  if (dispAovTable) {
    aovDispTable(ezObj, name = sys.call())
  }

  if (dispAovMeans) {
     aovDispMeans(ezObj, name = sys.call())
  }

  ezObj$aovTable = TRUE

  return(ezObj)

}
