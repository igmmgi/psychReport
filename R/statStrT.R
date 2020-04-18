#' @title statStrT
#'
#' @description Returns required Latex formatted string T-test required for R/Knitr integration.
#' For example, \emph{t}(11) = 3.45, \emph{p} < 0.05.
#' Returns values to 2 sig decimal places and < 0.01, < 0.001 for p values.
#'
#' @param tObj The returned object from a call to t.test
#'
#' @return character
#'
#' @examples
#' requiredPackages(c("dplyr"))
#'
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) levels
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 150, 100),
#'                                 "Comp_incomp" = c(520, 150, 100)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' tObj <- t.test(datAggVP$rt[datAggVP$Comp == "comp"],
#'                datAggVP$rt[datAggVP$Comp == "incomp"],
#'                paired = TRUE)
#'
#' statStrT <- statStrT(tObj)
#'
#' @export
statStrT <- function(tObj) {
  return(paste0(tValueString(tObj), ", ", pValueString(tObj$p.value)))
}
