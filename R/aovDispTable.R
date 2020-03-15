#' @title aovDispTable
#'
#' @description Display formatted ANOVA table in command window.
#'
#' @param ezObj Output from ezANOVA
#' @param caption Required for heading
#'
#' @return NULL
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
#' dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 150, 100),
#'                                 "Comp_incomp" = c(520, 150, 100)))
#'
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)
#' aovDispTable(aovRT)
#'
#' @export
aovDispTable <- function(ezObj, caption=sys.call()) {
  if (!is.character(caption)){
    caption <- paste0("ANOVA:", unlist(lapply(caption[2], as.character)))
  }
  width <- max(apply(ezObj$ANOVA, 1, function(x) sum(nchar(x))))
  print(cli::rule(line = 2, center = crayon::black(caption), width = width + 13))
  print(ezObj$ANOVA, row.names = FALSE)
  print(cli::rule(width = width + 13))
}
