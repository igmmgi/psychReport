#' @title statStrAov
#'
#' @description Returns Latex formatted string from ANOVA required for R/Sweave integration.
#' For example, \deqn{F(1, 20) = 8.45, p < 0.01, pes = 0.45}
#' Returns values to 2 sig decimal places and < 0.01, < 0.001 for p values.
#'
#' @param ezObj Output from ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effect The effect required from the anova table
#'
#' @return NULL
#'
#' @examples
#' library(psychReport)
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
#' aovString <- statStrAov(aovRT, "Comp")
#' aovString <- statStrAov(aovRT, "Comp:Side")
#'
#' \dontrun{
#' # Example use in *.Rnw file
#' # \Sexpr{aovString} }
#'
#' @export
statStrAov <- function(ezObj, effect) {

  fString <- fValueString(ezObj, effect)
  pString <- pValueString(ezObj$ANOVA[, "p"][ezObj$ANOVA$Effect == effect])
  eString <- effectsizeValueString(ezObj, effect)
  sString <- sphericityValueString(ezObj, effect)

  message(sString)
  if (is.null(sString)) {
     return(paste0(fString, ", ", pString, ", ", eString))
  } else {
    return(paste0(fString, ", ", pString, ", ", eString, ", ", sString))
  }

}
