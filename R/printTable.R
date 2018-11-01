#' @title printTable
#'
#' @description Returns Latex formatted table from dataframe or ezANOVA ANOVA table.
#' Uses xtable latex package with some basic defaults.
#' For more examples, see R package xtable
#' @param obj Dataframe/ezANOVA object to print
#' @param caption Title of the dataframe
#' @param digits Number of digits to round to
#' @param onlyContents TRUE/FALSE
#' @param formatStatsSymbols TRUE/FALSE
#'
#' @return character
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#' # Example 1:
#' # create dataframe
#' dat <- createDF(nVP = 6, nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list(list(c("Comp:comp"), vals = c(500, 150, 100)),
#'                                 list(c("Comp:incomp"), vals = c(520, 150, 100))))
#' printTable(dat) # latex formatted
#'
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#' printTable(aovRT$ANOVA) # latex formatted
#'
#' \dontrun{
#' # Example use in *.Rnw Sweave file inside R chunk
#' # << printTable, echo = FALSE, results = tex >>=
#' # printTable(aovRT$ANOVA, caption = "ANOVA Table")
#' # @}
#'
#' @export
printTable <- function(obj, caption = "DF", digits=3, onlyContents=FALSE,
                       formatStatsSymbols = TRUE) {

  # typical symbols in ANOVA table
  if (formatStatsSymbols) {
    names(obj) <- sub("\\<p\\>",   "\\\\textit{p}",   names(obj))
    names(obj) <- sub("\\<F\\>",   "\\\\textit{F}",   names(obj))
    names(obj) <- sub("\\<es\\>",  "$\\\\eta^2$",     names(obj))
    names(obj) <- sub("\\<pes\\>", "$\\\\eta_{p}^2$", names(obj))
    names(obj) <- sub("\\<ges\\>", "$\\\\eta_{G}^2$", names(obj))
    names(obj) <- sub("\\<eps\\>", "$\\\\epsilon$",   names(obj))
  }

  if (length(digits) > 1) {
    numCols            <- which(sapply(obj, is.numeric))
    tmpDigits          <- rep(0, ncol(obj))
    tmpDigits[numCols] <- digits
    digits             <- c(0, tmpDigits)
  }

  tab <- xtable::xtable(obj, caption = caption)
  tab <- xtable::autoformat(tab)
  xtable::digits(tab) <- digits

  print(tab,
        table.placement = "H",
        caption.placement = "top",
        include.rownames = FALSE,
        floating = FALSE,
        tabular.environment = "longtable",
        only.contents = onlyContents,
        sanitize.text.function = function(x){x})
}
