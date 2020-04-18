#' @title tValueString
#'
#' @description Returns required Latex formatted string for \emph{t}(df) = XXX for
#' R/Sweave integration. Returns values to 2 sig decimal places.
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
#'     summarize(N = n(),
#'               rt = mean(RT))
#'
#' tObj <- t.test(datAggVP$rt[datAggVP$Comp == "comp"],
#'                datAggVP$rt[datAggVP$Comp == "incomp"],
#'                paired = TRUE)
#'
#' tString <- tValueString(tObj)
#'
#' @export
tValueString <- function(tObj) {

  tVal   <- format(round(tObj$statistic, 2), nsmall = 2)
  DF     <- tObj$parameter
  if (DF %% 1 != 0) {
    DF <- format(round(DF, 2), nsmall = 2)
  }

  return(paste0("\\emph{t}", "(", DF, ") = ", tVal, sep = ""))

}
