#' @title aovSphericityAdjustment
#'
#' @description Adjust ezANOVA table with corrections for sphericity (Greenhouse-Geisser or
#' Huynh-Feldt). Called by default within aovTable
#'
#' @param ezObj The returned object from a call to ezANOVA
#' @param type "GG" (Greenhouse-Geisser) or "HF" (Huynh-Feldt)
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#' # Example 1:
#' # create dataframe with 3(Comp: neutral vs. comp vs. incomp) factors/levels
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("neutral", "comp", "incomp")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp_neutral" = c(510, 150, 100),
#'                            "Comp_comp"    = c(500, 150, 100),
#'                            "Comp_incomp"  = c(520, 150, 100)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp) %>%
#'     summarize(N = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovDispTable(aovRT)
#' aovRT <- aovSphericityAdjustment(aovRT)
#' aovDispTable(aovRT)
#'
#' @return list
#'
#' @export
aovSphericityAdjustment <- function(ezObj, type = "GG") {

  hasSphericity <- ezObj$"Sphericity Corrections"
  if (is.null(hasSphericity)) {
    return(ezObj)
  }

  sphericityRows <- match(rownames(ezObj$"Sphericity Corrections"), rownames(ezObj$ANOVA))
  if (type == "GG") {
    ezObj$ANOVA$p[sphericityRows]   <- ezObj$"Sphericity Corrections"$"p[GG]"
    ezObj$ANOVA$eps                 <- rep(0, length(ezObj$ANOVA$"Effect"))
    ezObj$ANOVA$eps[sphericityRows] <- ezObj$"Sphericity Corrections"$GGe
  } else if (type == "HF") {
    ezObj$ANOVA$p[sphericityRows]   <- ezObj$"Sphericity Corrections"$"p[HF]"
    ezObj$ANOVA$eps                 <- rep(0, length(ezObj$ANOVA$"Effect"))
    ezObj$ANOVA$eps[sphericityRows] <- ezObj$"Sphericity Corrections"$HFe
  } else {
    stop("Sphericity correction type not recognized!")
  }

  return(ezObj)

}
