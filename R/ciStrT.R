#' @title ciStrT
#'
#' @description Returns a string with the 95\% CI from a t.test in Latex format.
#'
#' @param tObj The returned object from a call to t.test
#' @param numDigits The number of digits to round to
#' @param unit "" vs. "ms" vs. "mv" vs. "\%"
#'
#' @return character
#'
#' @examples
#' requiredPackages(c("dplyr"))
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
#' ciString <- ciStrT(tObj, unit = "ms")
#'
#' \dontrun{
#' # Example use in *.Rnw Sweave file
#' # \Sexpr{ciString}}
#'
#' @export
ciStrT <- function(tObj, numDigits = 0, unit = "") {
  return(paste0("95\\% CI: ", numValueString(tObj$conf.int[1], numDigits, ""),
                " to ",    numValueString(tObj$conf.int[2], numDigits, unit)))
}
