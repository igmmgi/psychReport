#' @title pValueSummary
#'
#' @description Returns p-values summarized using ***, **, *, or exact value
#' when \emph{p} > .05 (default 2 significant decimal places).
#'
#' @param pVal vector with p-value between 0 and 1
#' @param nsmall Number of small digits to round to
#'
#' @return character
#'
#' @examples
#' # Example 1:
#' psum <- pValueSummary(0.0067)
#'
#' # Example 2:
#' psum <- pValueSummary(c(0.6712, 0.1, 0.0001), nsmall = 3)
#'
#' @export
pValueSummary <- function(pVal, nsmall = 2) {

  if (!is.numeric(pVal)) {
     stop("Input contains a non-number!")
  }

  psum <- ifelse(pVal < 0.001, "***",
                 ifelse(pVal < 0.01, "**",
                       ifelse(pVal < 0.05, "*",
                              gsub("0\\.", ".", format(round(pVal, digits = nsmall),
                                                       nsmall = nsmall)))))

  return(psum)

}
