#' @title fValueString
#'
#' @description Returns required Latex formatted string for \emph{F}(df1, df2) = XXX
#' for R/Sweave integration. For example, \emph{F}(1, 23) = 3.45.
#  Returns values to 2 sig decimal places.
#'
#' @param ezObj The returned object from a call to ezANOVA
#' @param effect The effect within the ANOVA table to return
#'
#' @return character
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
#' dat <- addDataDF(dat, RT = list("Comp:Side_comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side_comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side_incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side_incomp:right" = c(520, 150, 100)))
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
#' fString <- fValueString(aovRT, "Comp")
#' fString <- fValueString(aovRT, "Comp:Side")
#'
#' \dontrun{
#' # Example use in *.Rnw Sweave file
#' # \Sexpr{fString} }
#'
#' @export

fValueString <- function(ezObj, effect){
  DFn    <- ezObj$ANOVA[, "DFn"][ezObj$ANOVA$Effect == effect]
  DFd    <- ezObj$ANOVA[, "DFd"][ezObj$ANOVA$Effect == effect]
  fValue <- ezObj$ANOVA[, "F"][ezObj$ANOVA$Effect == effect]
  return(paste0("\\emph{F}", "(", DFn, ", ", DFd, ") = ", fValue))
}
