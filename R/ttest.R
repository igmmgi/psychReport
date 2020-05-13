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



#' @title tValueString
#'
#' @description Returns required Latex formatted string for \emph{t}(df) = XXX for
#' R/knitr integration. Returns values to 2 sig decimal places.
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
#' dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 150, 100),
#'                                 "Comp_incomp" = c(520, 150, 100)))
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
#' @export
meanStrT <- function(tObj, numDigits = 0, unit = "") {
  if (length(tObj$estimate) == 1) {
    return(paste0(numValueString(tObj$estimate[1], numDigits, unit)))
  } else {
    return(paste0(numValueString(tObj$estimate[1], numDigits, ""),
                " vs. ",    numValueString(tObj$estimate[2], numDigits, unit)))
  }
}



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
#' ciString <- ciStrT(tObj, unit = "ms")
#'
#' @export
ciStrT <- function(tObj, numDigits = 0, unit = "") {
  return(paste0("95\\% CI: ", numValueString(tObj$conf.int[1], numDigits, ""),
                " to ",    numValueString(tObj$conf.int[2], numDigits, unit)))
}
