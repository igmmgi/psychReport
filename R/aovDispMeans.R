#' @title aovDispMeans
#'
#' @description Displays marginal means from model.tables in the command window.
#'
#' @param ezObj Output from ezANOVA  (NB. ezANOVA must be called with \"return_aov = TRUE\"")
#' @param value String for column name
#' @param caption Required for heading
#'
#' @return NULL
#'
#' @examples
#' library(psychReport)
#' requiredPackages(c("ez"))
#' # Example 1:
#' # create dataframe
#' dat <- createDF(nVP = 50,
#'                 nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list(list(c("Comp:comp"), vals = c(500, 100, 100)),
#'                                 list(c("Comp:incomp"), vals = c(520, 100, 100))))
#'
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#' aovDispMeans(aovRT)
#'
#' @export
aovDispMeans <- function(ezObj, value="value", caption=sys.call()) {

  if (is.null(ezObj$means)) {
    stop("ezANOVA object does not contain marginal means!\nCall ezANOVA with \"return_aov = TRUE\"")
  } else {
    means = ezObj$means
  }

  for (i in 2:(length(means$n) + 1)) {

    dat <- as.data.frame.table(means$tables[[i]], responseName = value)

    heading <- paste0(c(row.names(as.data.frame(means$n)))[i - 1])
    width <- max(nchar(caption), nchar(heading), apply(dat, 1, function(x) sum(nchar(x)))) + 8
    if (i == 2) {
      if (!is.character(caption)){
        caption <- paste0("ANOVA:", unlist(lapply(caption[2], as.character)))
      }
      print(cli::rule(line = 2, center = crayon::black(caption), width = width))
    }
    print(cli::rule(center = crayon::black(heading), width = width))
    print(dat, row.names = FALSE)
    cat("\n")
  }
  print(cli::rule(width = width))
}
