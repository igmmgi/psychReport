#' @title printAovMeans
#'
#' @description Returns Latex formatted table of marginal means from model.tables.
#' Uses printTable (xtable) latex package with some basic defaults.
#' For more examples, see R package xtable
#' @param ... Output from ezANOVA  (NB. ezANOVA must be called with \"return_aov = TRUE\"")
#' @param caption Title for the table
#' @param digits Number of digits to round to
#' @param dv Name of the dependent variable (e.g., "ms", "\%")
#'
#' @return character
#'
#' @examples
#' library(psychReport)
#' requiredPackages(c("ez"))
#' # Example 1:
#' # create dataframe
#' dat <- createDF(nVP = 6,
#'                 nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list(list(c("Comp:comp"), vals = c(500, 150, 100)),
#'                                 list(c("Comp:incomp"), vals =c(520, 150, 100))))
#'
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#' printAovMeans(aovRT, digits = 0, dv = "mV")  # latex formatted
#'
#' \dontrun{
#' # Example use in *.Rnw Sweave file inside R chunk
#' # << printTable, echo = FALSE, results = tex >>=
#' # printAovMeans(aovRT, caption = "ANOVA Table")
#' # @}
#'
#' @export
printAovMeans <- function(..., caption = "Mean", digits = 3, dv = "ms") {

  ezObj <- list(...)
  for (i in seq(1:length(ezObj))) {
    if (is.null(ezObj[[i]]$means)) {
      stop("ezANOVA object does not contain marginal means!\nCall ezANOVA with \"return_aov = TRUE\"")
    }
  }
  if (!length(digits) %in% c(1, length(ezObj))) {
    stop("length digits must equal 1 or number of ezObj inputs")
  }
  if (!length(dv) %in% c(1, length(ezObj))) {
    stop("dv length must equal 1 or number of ezObj inputs")
  }

  # format some common Latex strings within the caption label
  caption <- gsub("%", "\\%",      caption, fixed = TRUE)
  caption <- gsub("_", "\\_",      caption, fixed = TRUE)
  caption <- gsub("mV", "$\\mu$V", caption, fixed = TRUE)

  # format some common Latex strings within the dv label
  dv <- gsub("%", "\\%",      dv, fixed = TRUE)
  dv <- gsub("_", "\\_",      dv, fixed = TRUE)
  dv <- gsub("mV", "$\\mu$V", dv, fixed = TRUE)

  for (i in 2:(length(ezObj[[1]]$means$n) + 1)) {

    tab <- as.data.frame.table(ezObj[[1]]$means$tables[[i]])
    names(tab)[ncol(tab)] <- dv[1]
    for (j in 1:length(ezObj)) {
      tab <- cbind(tab, as.data.frame.table(ezObj[[j]]$means$tables[[i]]))
      names(tab)[ncol(tab)] <- dv[j]
    }

    tab <- tab[, !duplicated(colnames(tab))]
    printTable(tab,
               caption = paste0(caption, ": ", names(ezObj[[1]]$means$tables[i])),
               digits = digits)
  }

}

