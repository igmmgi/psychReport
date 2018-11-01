#' @title meanStrT
#'
#' @description Returns a string with the mean value from a t.test in Latex format.
#'
#' @param tObj The returned object from a call to t.test
#' @param numDigits The number of digits to round to
#' @param unit "" vs. "ms" vs. "mv" vs. "\%"
#'
#' @return character
#'
#' @examples
#' library(psychReport)
#' requiredPackages(c("dplyr"))
#' # Example 1:
#' # create dataframe and add data
#' dat <- createDF(nVP = 10,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list(list(c("Comp:comp"), vals = c(500, 150, 100)),
#'                                 list(c("Comp:incomp"), vals = c(520, 150, 100))))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp) %>%
#'     summarize(N = n(),
#'               rt = mean(RT))
#'
#' tObj <- t.test(datAggVP$rt[dat$Comp == "comp"],
#'                datAggVP$rt[dat$Comp == "incomp"],
#'                paired = TRUE)
#'
#' tString <- meanStrT(tObj, numDigits = 0, unit = "ms")
#'
#' \dontrun{
#' # Example use in *.Rnw Sweave file
#' # \Sexpr{tString} }
#'
#' @export
meanStrT <- function(tObj, numDigits = 0, unit = "") {
  if (length(tObj$estimate) == 1) {
    return(paste0(numValueString(tObj$estimate[1], numDigits, unit)))
  } else {
    return(paste0(numValueString(tObj$estimate[1], numDigits, ""),
                " vs. ",    numValueString(tObj$estimate[2], numDigits, unit)))
  }
}
