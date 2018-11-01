#' @title meanStrAov
#'
#' @description Returns marginal means from ezANOVA object for requested effect in Latex format.
#' Assumes means added to aovObj (e.g., aovObj$means <- model.tables(aovObj$aov, type = "mean").
#'
#' @param ezObj Output from ezANOVA called with "return_aov = TRUE"
#' @param effect Effect to return
#' @param level Level of effect
#' @param unit "ms" vs. "mv" vs. "\%"
#' @param numDigits "ms" vs. "mv" vs. "\%"
#'
#' @return character
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat, RT = list(list(c("Comp:comp", "Side:left"), vals = c(500, 150, 100)),
#'                                 list(c("Comp:comp", "Side:right"), vals = c(500, 150, 100)),
#'                                 list(c("Comp:incomp", "Side:left"), vals = c(520, 150, 100)),
#'                                 list(c("Comp:incomp", "Side:right"), vals = c(520, 150, 100))))
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
#' aovRT <- aovTable(aovRT)
#'
#' meanString <- meanStrAov(aovRT, "Comp", "comp")
#' meanString <- meanStrAov(aovRT, "Comp:Side", "incomp:left")
#'
#' \dontrun{
#' # Example use in *.Rnw Sweave file
#' # \Sexpr{meanString} }
#'
#' @export
meanStrAov <- function(ezObj, effect, level, unit = "ms", numDigits = 0) {

  row <- which(ezObj$ANOVA$Effect == effect) + 1
  dat <- as.data.frame.table(ezObj$means$tables[[row]], responseName = "DV")

  effect <- unlist(strsplit(effect, ":"))
  level  <- unlist(strsplit(level, ":"))

  out <- matrix(0, nrow(dat), length(effect))
  for (i in 1:length(effect)) {
    out[, i] <- dat[, effect[i]] == level[i]
  }

  if (length(effect) == 1) {
    reqRow <- which(out[, 1] == 1)
  } else {
    reqRow <- which(rowSums(out[, 1:length(effect)]) == length(effect))
  }

  return(numValueString(dat[reqRow, "DV"], unit = unit, numDigits = numDigits))

}
