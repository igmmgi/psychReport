#' @title mathString
#'
#' @description Returns formatted string following addition/subtraction.
#'
#' @param str1 string
#' @param str2 string
#' @param operation "+", "-", "*", "/"
#' @param numDigits number 0 (default)
#' @param unit "ms" , "mV" , "mv", or "\%"
#'
#' @return NULL
#'
#' @examples
#' # Example 1:
#' string <- mathString("550 ms", "480 ms", "-")
#'
#' # Example 2:
#' string <- mathString("2.34", "1.65", "+", numDigits = 2, unit = "mV")
#'
#' @export
mathString <- function(str1, str2, operation = "-",
                       numDigits = 0, unit = "ms") {

  extractNum <- function(x){
    return(as.numeric(regmatches(x, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", x))))
  }

  nums <- lapply(list(str1, str2), extractNum)
  result <- do.call(operation, nums)

  return(numValueString(result, numDigits, unit))

}
