#' @title pValueString
#'
#' @description Returns Latex formatted string from a p-value required for R/Sweave integration.
#' For example, \emph{p} = 0.11 or \emph{p} < 0.01
#' Returns values to 2 sig decimal places if p-value >= 0.05.
#'
#' @param pVal p-value between 0 and 1
#' @param nsmall Number of small digits to round to
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' pString <- pValueString(0.67)
#'
#' # Example 2:
#' pString <- pValueString(0.1234, 3)
#'
#' # Example 3:
#' pString <- pValueString("0.03")
#'
#' \dontrun{
#' # Example use in *.Rnw Sweave file
#' # \Sexpr{pString} }
#'
#' @export
pValueString <- function(pVal, nsmall = 2){

  if (is.character(pVal)) {
     pVal <- as.numeric(pVal)
     if (is.na(pVal)) {
      stop("Can't convert string to number!")
     }
  }

  if (pVal >= 0.01) {
    string <- paste0("\\emph{p} = ", format(round(pVal, nsmall), nsmall = nsmall))
    string <- gsub("0\\.", ".", string)
  } else if (pVal >= 0.001 & pVal < 0.01) {
    string <- paste0("\\emph{p}", " < .01")
  } else if (pVal < 0.001) {
    string <- paste0("\\emph{p}", " < .001")
  }

  return(string)

}
