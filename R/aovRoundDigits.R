#' @title aovRoundDigits
#'
#' @description Round digits to n decimal places in ezANOVA table
#'
#' @param ezObj Output from ezANOVA
#' @param nsmall Number of small digits to round to within ANOVA table
#'
#' @return dataframe
#'
#' @examples
#' library(psychReport)
#' requiredPackages(c("dplyr", "ez"))
#' # Example 1:
#' # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp:Side_comp:left"    = c(500, 150, 150),
#'                            "Comp:Side_comp:right"   = c(500, 150, 150),
#'                            "Comp:Side_incomp:left"  = c(500, 150, 150),
#'                            "Comp:Side_incomp:right" = c(500, 150, 150)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovDispTable(aovRT)
#' aovRT <- aovRoundDigits(aovRT, 2)
#' aovDispTable(aovRT)
#'
#' @export
aovRoundDigits <- function(ezObj,
                           nsmall = 2) {

  colNames <- c("SSn", "SSd", "F", "p", "eps", "ges", "es", "pes")
  colIdx   <- which(names(ezObj$ANOVA) %in% colNames)
  ezObj$ANOVA[, colIdx] <- format(round(ezObj$ANOVA[, colIdx], nsmall), nsmall = nsmall)

  return(ezObj)

}
