#' @title numValueString
#'
#' @description Returns numerical value with requested unit in Latex format with numDigits
#' number of decimal places and unit symbol.
#'
#' @param value number
#' @param numDigits number 2 (default)
#' @param unit "ms", "mv", "mV", or "\%" or "" (default)
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' string <- numValueString(100.341, 0, "ms")
#'
#' # Example 2:
#' string <- numValueString(2.3412, 2, "mv")
#'
#' # Example 3:
#' string <- numValueString(63.9812, 2, "")
#'
#' @export
numValueString <- function(value, numDigits = 2, unit = "") {

  value <- format(round(value, numDigits), nsmall = numDigits)
  if (unit %in% c("mv", "mV")) {
    return(paste0(value, " $\\\\mu$V"))
  } else if (unit == "ms") {
    return(paste0(value, " ms"))
  } else if (unit == "%") {
    return(paste0(value, " \\\\%"))
  } else if (unit == "") {
    return(paste0(value))
  } else {
    stop("Unit not recognized! Unit should be \"mv\", \"mV\", \"ms\" or \"%\".")
  }

}
