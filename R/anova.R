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
#' # Example 1:
#' requiredPackages(c("ez"))
#' # create dataframe
#' dat <- createDF(nVP = 6,
#'                 nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 150, 100),
#'                                 "Comp_incomp" = c(520, 150, 100)))
#'
#' dat$VP <- as.factor(dat$VP)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)
#' aovDispTable(aovRT)
#'
#' @export
aovDispTable <- function(ezObj, caption=sys.call()) {
  if (!is.character(caption)) {
    caption <- paste0("ANOVA:", unlist(lapply(caption[2], as.character)))
  }
  width <- max(apply(ezObj$ANOVA, 1, function(x) sum(nchar(x))))
  print(cli::rule(line = 2, center = crayon::black(caption), width = width + 13))
  print(ezObj$ANOVA, row.names = FALSE)
  print(cli::rule(width = width + 13))
}



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
#' requiredPackages(c("ez"))
#'
#' # Example 1:
#' # create dataframe
#' dat <- createDF(nVP = 50,
#'                 nTrl = 1,
#'                 design = list("Comp" = c("comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 100, 100),
#'                                 "Comp_incomp" = c(520, 100, 100)))
#'
#' dat$VP <- as.factor(dat$VP)
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
    heading <- names(means$tables[i])
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



#' @title aovEffectSize
#'
#' @description Add partial eta squared (pes) or eta-squared (es) effect size measures
#' to ezANOVA table.
#'
#' @param ezObj Output from ezANOVA
#' @param effectSize "ges" vs. pes"
#'
#' @return list
#'
#' @examples
#' requiredPackages(c("ez", "dplyr"))
#'
#' # Example 1:
#' # create dataframe with 2(Comp: comp vs. incomp) and 2(Side: left vs. right) factors/levels
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp", "neutral"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat,
#'                  RT = list("Comp:Side_comp:left"     = c(500, 150, 150),
#'                            "Comp:Side_comp:right"    = c(500, 150, 150),
#'                            "Comp:Side_incomp:left"   = c(550, 150, 150),
#'                            "Comp:Side_incomp:right"  = c(550, 150, 150),
#'                            "Comp:Side_neutral:left"  = c(525, 150, 150),
#'                            "Comp:Side_neutral:right" = c(525, 150, 150)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovEffectSize(aovRT, "pes")
#' aovRT <- aovTable(aovRT)
#'
#' @export
aovEffectSize <- function(ezObj, effectSize) {

   if (effectSize == "ges") {
    # NB assumes no observed variables within initial call to ezANOVA!
    ezObj$ANOVA$ges <- ezObj$ANOVA$SSn / (ezObj$ANOVA$SSn + sum(unique(ezObj$ANOVA$SSd)))
    ezObj$ANOVA$pes <- NULL
  } else if (effectSize == "pes") {
    ezObj$ANOVA$ges <- NULL
    ezObj$ANOVA$pes <- ezObj$ANOVA$SSn / (ezObj$ANOVA$SSn + ezObj$ANOVA$SSd)
  } else {
    stop("effectSize not recognized!")
  }

  return(ezObj)

}



#' @title adjustJackknifeAdjustment
#'
#' @description Adjust ezANOVA table with corrected F (Fc = F/(n-1)^2) and p values for jackkniffed data (see Ulrich and Miller, 2001. Using the jackknife-based scoring method for measuring LRP onset effects in factorial designs. Psychophysiology, 38, 816-827.)
#'
#' @param ezObj Output from ezANOVA
#' @param numVPs The number of participants
#'
#' @return list
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#'
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
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovDispTable(aovRT)
#' aovRT <- aovJackknifeAdjustment(aovRT, length(unique(datAggVP$VP)))
#' aovDispTable(aovRT)
#'
#' @export
aovJackknifeAdjustment <- function(ezObj, numVPs) {

  ezObj$ANOVA$SSd     <- ezObj$ANOVA$SSd*((numVPs - 1) ^ 2)
  ezObj$ANOVA$F       <- ezObj$ANOVA$F/((numVPs - 1) ^ 2)
  ezObj$ANOVA$p       <- 1 - stats::pf(ezObj$ANOVA$F, ezObj$ANOVA$DFn, ezObj$ANOVA$DFd)
  ezObj$ANOVA$"p<.05" <- pValueSummary(ezObj$ANOVA$p)

  return(ezObj)

}



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
#' datAggVP$VP <- as.factor(datAggVP$VP)
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



#' @title aovSphericityAdjustment
#'
#' @description Adjust ezANOVA table with corrections for sphericity (Greenhouse-Geisser or
#' Huynh-Feldt). Called by default within aovTable
#'
#' @param ezObj The returned object from a call to ezANOVA
#' @param type "GG" (Greenhouse-Geisser) or "HF" (Huynh-Feldt)
#'
#' @return list
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#'
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
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovDispTable(aovRT)
#' aovRT <- aovSphericityAdjustment(aovRT)
#' aovDispTable(aovRT)
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



#' @title aovTable
#'
#' @description Adjust ezANOVA table output. Options include calculation of alternative
#' effect sizes (eta squared, partial eta squared), the calculation of marginal
#' means and formating options for the ANOVA table (e.g., detailed, rounding).
#'
#' @param ezObj Output from ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effectSize "ges" (generalized eta-squared), "es" (eta-squared), or
#' "pes" (partial eta-squared)
#' @param sphericityCorrections TRUE/FALSE
#' @param sphericityCorrectionType "GG" (default) vs. "HF"
#' @param marginalMeans Return marginal means via model.tables (NB. ezANOVA must
#' be called with return_aov = TRUE)
#' @param removeSumSquares TRUE/FALSE Remove SSn/SSd columns from the ANOVA table
#' @param removeIntercept TRUE/FALSE Remove intercept row from the ANOVA table
#' @param roundDigits TRUE/FALSE Round numerical values to numDigits
#' @param numDigits The number of digits to round to if roundDigits = TRUE
#' @param dispAovTable Display the ANOVA table in the command window
#' @param dispAovMeans Display the marginal means in the command window
#' @param caption Table caption
#'
#' @return list
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#'
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
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' @export
aovTable <- function(ezObj,
                     effectSize = "pes",
                     sphericityCorrections = TRUE,
                     sphericityCorrectionType = "GG",
                     marginalMeans = TRUE,
                     removeSumSquares = TRUE,
                     removeIntercept = TRUE,
                     roundDigits = TRUE,
                     numDigits = 2,
                     dispAovTable = TRUE,
                     dispAovMeans = FALSE,
                     caption = NULL) {

  if (!"SSn" %in% names(ezObj$ANOVA)) {
    stop("Call ezANOVA with \"detailed = TRUE\"!")
  }

  if (!"aov" %in% names(ezObj)) {
      stop("Call ezANOVA with \"return_aov = TRUE\"!")
  }

  if (effectSize != "ges") {
      ezObj <- aovEffectSize(ezObj, effectSize)
  }

  if (sphericityCorrections) {
    ezObj <- aovSphericityAdjustment(ezObj, sphericityCorrectionType)
  }

  if (removeIntercept) {
    ezObj$ANOVA <- ezObj$ANOVA[-c(1), ]
  }

  # p-value summary *** vs. ** vs *
  ezObj$ANOVA$"p<.05" <- pValueSummary(ezObj$ANOVA$p)

  if (roundDigits) {
    ezObj <- aovRoundDigits(ezObj, nsmall = numDigits)
  }

  if (removeSumSquares) {
    ezObj$ANOVA$SSn <- NULL
    ezObj$ANOVA$SSd <- NULL
  }

  if (marginalMeans) {
    ezObj$means <- stats::model.tables(ezObj$aov, type = "mean")
  }

  if (dispAovTable) {
      if (is.null(caption)) {
          aovDispTable(ezObj, caption = sys.call())
      } else {
          aovDispTable(ezObj, caption = caption)
      }
  }

  if (dispAovMeans) {
      if (is.null(caption)) {
          aovDispMeans(ezObj, caption = sys.call())
      } else {
          aovDispMeans(ezObj, caption = caption)
      }
  }

  ezObj$aovTable = TRUE

  return(ezObj)

}



#' @title effectsizeValueString
#'
#' @description Returns required Latex formatted string for effect size (eta squared, partial
#' eta square, generalized eta squared) = XXX for R/Sweave integration.
#  Returns values to 2 sig decimal places.

#' @param ezObj The returned object from a call to ezANOVA
#' @param effect The effect within the ANOVA table to return
#' @param effectSize The effect size to report ("ges" default witin ezANOVA, "pes" partial eta squared)
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
#' dat <- addDataDF(dat, RT = list("Comp:Side_comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side_comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side_incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side_incomp:right" = c(520, 150, 100)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' pesString <- effectsizeValueString(aovRT, "Comp")  # partial eta squared
#' pesString <- effectsizeValueString(aovRT, "Comp:Side")
#'
#' @export

effectsizeValueString <- function(ezObj, effect, effectSize = "pes"){

  effectSizeIdx <- which(names(ezObj$ANOVA) %in% effectSize)
  if (length(effectSizeIdx) == 0) {
    stop("Requested effect size not present in ANOVA table!")
  }
  if (effectSize == "ges") {
    effectSizeValue <- ezObj$ANOVA[, "ges"][ezObj$ANOVA$Effect == effect]
    return(paste0("$\\eta_{G}^2$ = ", effectSizeValue))
  } else if (effectSize == "pes") {
    effectSizeValue  <- ezObj$ANOVA[, "pes"][ezObj$ANOVA$Effect == effect]
    return(paste0("$\\eta_{p}^2$ = ", effectSizeValue))
  }
}



#' @title fValueString
#'
#' @description Returns required Latex formatted string for \emph{F}(df1, df2) = XXX
#' for R/Sweave integration. For example, \emph{F}(1, 23) = 3.45.
#' Returns values to 2 sig decimal places.
#'
#' @param ezObj The returned object from a call to ezANOVA
#' @param effect The effect within the ANOVA table to return
#'
#' @ return character
#'
#' @ examples
#' library(psychReport)
#' requiredPackages(c("dplyr", "ez"))
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat, RT = list("Comp:Side_comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side_comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side_incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side_incomp:right" = c(520, 150, 100)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' fString <- fValueString(aovRT, "Comp")
#' fString <- fValueString(aovRT, "Comp:Side")
#'
#' @export

fValueString <- function(ezObj, effect){
  DFn    <- ezObj$ANOVA[, "DFn"][ezObj$ANOVA$Effect == effect]
  DFd    <- ezObj$ANOVA[, "DFd"][ezObj$ANOVA$Effect == effect]
  fValue <- ezObj$ANOVA[, "F"][ezObj$ANOVA$Effect == effect]
  return(paste0("\\emph{F}", "(", DFn, ", ", DFd, ") = ", fValue))
}



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
#' dat <- addDataDF(dat, RT = list("Comp:Side_comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side_comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side_incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side_incomp:right" = c(520, 150, 100)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' meanString <- meanStrAov(aovRT, "Comp", "comp")
#' meanString <- meanStrAov(aovRT, "Comp:Side", "incomp:left")
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
#' dat <- addDataDF(dat, RT = list("Comp_comp"   = c(500, 150, 100),
#'                                 "Comp_incomp" = c(520, 150, 100)))
#'
#' dat$VP <- as.factor(dat$VP)
#' aovRT <- ezANOVA(dat, dv=.(RT), wid = .(VP), within = .(Comp), return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#' printAovMeans(aovRT, digits = 0, dv = "mV")  # latex formatted
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



#' @title statStrAov
#'
#' @description Returns Latex formatted string from ANOVA required for R/Sweave integration.
#' For example, \deqn{F(1, 20) = 8.45, p < 0.01, pes = 0.45}
#' Returns values to 2 sig decimal places and < 0.01, < 0.001 for p values.
#'
#' @param ezObj Output from ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effect The effect required from the anova table
#'
#' @return NULL
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#'
#' # Example 1:
#' # create dataframe and add data with 2(Comp: comp vs. incomp) and 2(Side: left vs. right)
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("comp", "incomp"),
#'                               "Side" = c("left", "right")))
#'
#' dat <- addDataDF(dat, RT = list("Comp:Side_comp:left"    = c(500, 150, 100),
#'                                 "Comp:Side_comp:right"   = c(500, 150, 100),
#'                                 "Comp:Side_incomp:left"  = c(520, 150, 100),
#'                                 "Comp:Side_incomp:right" = c(520, 150, 100)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp, Side) %>%
#'     summarize(N  = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp, Side),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' aovString <- statStrAov(aovRT, "Comp")
#' aovString <- statStrAov(aovRT, "Comp:Side")
#'
#' @export
statStrAov <- function(ezObj, effect) {

  fString <- fValueString(ezObj, effect)
  pString <- pValueString(ezObj$ANOVA[, "p"][ezObj$ANOVA$Effect == effect])
  eString <- effectsizeValueString(ezObj, effect)
  sString <- sphericityValueString(ezObj, effect)

  if (is.null(sString)) {
     return(paste0(fString, ", ", pString, ", ", eString))
  } else {
    return(paste0(fString, ", ", pString, ", ", eString, ", ", sString))
  }

}



#' @title sphericityValueString
#'
#' @description Returns required Latex formatted string for sphericity epsilon
#' values (HF, GG) = XXX for R/Sweave integration. Returns values
#' to 2 sig decimal places.
#'
#' @param ezObj The returned object from a call to ezANOVA
#' @param effect The effect within the ANOVA table to return
#'
#' @return character
#'
#' @examples
#' requiredPackages(c("dplyr", "ez"))
#'
#' # Example 1
#' # create dataframe and add data with 3(Comp: neutral vs. comp vs. incomp) levels
#' dat <- createDF(nVP = 20,
#'                 nTrl = 50,
#'                 design = list("Comp" = c("neutral", "comp", "incomp")))
#'
#' dat <- addDataDF(dat, RT = list("Comp_neutral" = c(510, 150, 100),
#'                                 "Comp_comp"    = c(500, 150, 100),
#'                                 "Comp_incomp"  = c(520, 150, 100)))
#'
#' # aggregate dat across trials
#' datAggVP <- dat %>%
#'     group_by(VP, Comp) %>%
#'     summarize(N = n(),
#'               rt = mean(RT))
#'
#' # repeated measures ANOVA using ezANOVA
#' datAggVP$VP <- as.factor(datAggVP$VP)
#' aovRT <- ezANOVA(datAggVP, dv=.(rt), wid = .(VP), within = .(Comp),
#'                  return_aov = TRUE, detailed = TRUE)
#' aovRT <- aovTable(aovRT)
#'
#' sphericityValue <- sphericityValueString(aovRT, "Comp")
#'
#' @export
sphericityValueString <- function(ezObj, effect){

  sphericityString = NULL
  if ("eps" %in% names(ezObj$ANOVA)) {
    if (ezObj$ANOVA[, "DFn"][ezObj$ANOVA$Effect == effect] != 1) {
      epsValue <- ezObj$ANOVA[, "eps"][ezObj$ANOVA$Effect == effect]
      sphericityString <- paste0("$\\epsilon$ = ", epsValue)
    }
  }
  return(sphericityString)
}
