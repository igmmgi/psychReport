#' @title sphericityValueString
#'
#' @description Returns required Latex formatted string for sphericity epsilon
#' values (HF, GG) = XXX for R/Sweave integration. Returns values
#' to 2 sig decimal places.
#'
#' @param ezObj The returned object from a call to ezANOVA
#' @param effect The effect within the ANOVA table to return
#'
#' @return character
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#' # Example 1
#' # create dataframe and add data with 3(Comp: neutral vs. comp vs. incomp) levels
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Side" = c("left", "right"), "Comp" = c("neutral", "comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list(list(c("Comp:neutral"), vals = c(510, 150, 100)),
#'                                 list(c("Comp:comp"), vals = c(500, 150, 100)),
#'                                 list(c("Comp:incomp"), vals = c(520, 150, 100))))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' # adjustAovTableSphericity called by default within adjustAovTableOptions
#' aovRT <- aovTable(aovRT)
#'
#' sphericityValue <- sphericityValueString(aovRT, "Comp")
#' sphericityValue <- sphericityValueString(aovRT, "Side")
#'
#' \dontrun{
#' # Example use in *.Rnw file
#' # \Sexpr{sphericityValue} }
#'
#' @export
sphericityValueString <- function(ezObj, effect){

  sphericityString = NULL
  if ("eps" %in% names(ezObj$ANOVA)) {
    if (ezObj$ANOVA[, "DFn"][ezObj$ANOVA$Effect == effect] != 1) {
      epsValue <- ezObj$ANOVA[, "eps"][ezObj$ANOVA$Effect == effect]
      sphericityString <- paste0("$\\epsilon$ = ", epsValue)
    }
  }
  return (sphericityString)
}
