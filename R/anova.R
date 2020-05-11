#' @title aovTidyTable
#'
#' @description Take output from base aov function and produce a "tidy" ANOVA table
#' similar to the output of ezANOVA
#'
#' @param aovOb Output from aov function
#'
#' @return aov
#'
#' @export
aovTidyTable <- function(aovObj) {
  aovTable        <- broom::tidy(aovObj)
  aovTable        <- aovTable[2:nrow(aovTable), 2:7]
  aovTable        <- cbind(aovTable[seq(1, nrow(aovTable), 2),], aovTable[seq(2, nrow(aovTable), 2), c(2,3,4)])
  aovTable        <- aovTable[, c(1,2,7,4,8,5,6)]
  names(aovTable) <- c("Effect", "DFn", "DFd", "SSn", "SSd", "F", "p")
  aovObj$means    <- stats::model.tables(aovObj, type = "mean")
  aovObj$ANOVA    <- aovTable
  return(aovObj)
}


#' @title aovDispTable
#'
#' @description Display formatted ANOVA table in command window.
#'
#' @param aovObj Output from aov or ezANOVA
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
#' aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
#' aovDispTable(aovRT)
#'
#' @export
aovDispTable <- function(aovObj, caption=sys.call()) {

  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  if (!is.character(caption)) {
    caption <- paste0("ANOVA:", unlist(lapply(caption[2], as.character)))
  }
  width <- max(apply(aovObj$ANOVA, 1, function(x) sum(nchar(x))))
  print(cli::rule(line = 2, center = crayon::black(caption), width = width + 13))
  print(aovObj$ANOVA, row.names = FALSE)
  print(cli::rule(width = width + 13))

}



#' @title aovDispMeans
#'
#' @description Displays marginal means from model.tables in the command window.
#'
#' @param aovObj Output from aov or ezANOVA  (NB. ezANOVA must be called with \"return_aov = TRUE\"")
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
#' aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
#' aovDispTable(aovRT)
#' aovDispMeans(aovRT)
#'
#' @export
aovDispMeans <- function(aovObj, value="value", caption=sys.call()) {


  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }
    means <- aovObj$means

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
#' to anova table.
#'
#' @param aovObj Output from aov or ezANOVA
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
#' aovRT <- aov(rt ~ Comp * Side + Error(VP/(Comp*Side)), datAggVP)
#' aovRT <- aovEffectSize(aovRT, "pes")
#'
#' @export
aovEffectSize <- function(aovObj, effectSize) {

  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  if (effectSize == "ges") {
    # NB assumes no observed variables within initial call to ezANOVA!
    aovObj$ANOVA$ges <- aovObj$ANOVA$SSn / (aovObj$ANOVA$SSn + sum(unique(aovObj$ANOVA$SSd)))
    aovObj$ANOVA$pes <- NULL
  } else if (effectSize == "pes") {
    aovObj$ANOVA$ges <- NULL
    aovObj$ANOVA$pes <- aovObj$ANOVA$SSn / (aovObj$ANOVA$SSn + aovObj$ANOVA$SSd)
  } else {
    stop("effectSize not recognized!")
  }

  return(aovObj)

}



#' @title adjustJackknifeAdjustment
#'
#' @description Adjust ezANOVA table with corrected F (Fc = F/(n-1)^2) and p values for jackkniffed data (see Ulrich and Miller, 2001.
#' Using the jackknife-based scoring method for measuring LRP onset effects in factorial designs. Psychophysiology, 38, 816-827.)
#'
#' @param aovObj Output from aov or ezANOVA
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
#' aovRT <- aov(rt ~ Comp*Side + Error(VP/(Comp*Side)), datAggVP)
#' aovRT <- aovJackknifeAdjustment(aovRT, length(unique(datAggVP$VP)))
#'
#' @export
aovJackknifeAdjustment <- function(aovObj, numVPs) {

  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  aovObj$ANOVA$SSd     <- aovObj$ANOVA$SSd*((numVPs - 1) ^ 2)
  aovObj$ANOVA$F       <- aovObj$ANOVA$F/((numVPs - 1) ^ 2)
  aovObj$ANOVA$p       <- 1 - stats::pf(aovObj$ANOVA$F, aovObj$ANOVA$DFn, aovObj$ANOVA$DFd)
  aovObj$ANOVA$"p<.05" <- pValueSummary(aovObj$ANOVA$p)

  return(aovObj)

}


#' @title aovRoundDigits
#'
#' @description Round digits to n decimal places in ezANOVA table
#'
#' @param aovObj Output from aov or ezANOVA
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
#' aovRT <- aov(rt ~ Comp*Side + Error(VP/(Comp*Side)), datAggVP)
#' aovRT <- aovRoundDigits(aovRT, 2)
#'
#' @export
aovRoundDigits <- function(aovObj, nsmall = 2) {

  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  colNames <- c("SSn", "SSd", "F", "p", "eps", "ges", "es", "pes")
  colIdx   <- which(names(aovObj$ANOVA) %in% colNames)
  aovObj$ANOVA[, colIdx] <- format(round(aovObj$ANOVA[, colIdx], nsmall), nsmall = nsmall)

  return(aovObj)

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
aovSphericityAdjustment <- function(aovObj, type = "GG") {

  hasSphericity <- aovObj$"Sphericity Corrections"
  if (is.null(hasSphericity)) {
    return(aovObj)
  }

  sphericityRows <- match(rownames(aovObj$"Sphericity Corrections"), rownames(aovObj$ANOVA))
  if (type == "GG") {
    aovObj$ANOVA$p[sphericityRows]   <- aovObj$"Sphericity Corrections"$"p[GG]"
    aovObj$ANOVA$eps                 <- rep(0, length(aovObj$ANOVA$"Effect"))
    aovObj$ANOVA$eps[sphericityRows] <- aovObj$"Sphericity Corrections"$GGe
  } else if (type == "HF") {
    aovObj$ANOVA$p[sphericityRows]   <- aovObj$"Sphericity Corrections"$"p[HF]"
    aovObj$ANOVA$eps                 <- rep(0, length(aovObj$ANOVA$"Effect"))
    aovObj$ANOVA$eps[sphericityRows] <- aovObj$"Sphericity Corrections"$HFe
  } else {
    stop("Sphericity correction type not recognized!")
  }

  return(aovObj)

}



#' @title aovTable
#'
#' @description Adjust ezANOVA table output. Options include calculation of alternative
#' effect sizes (eta squared, partial eta squared), the calculation of marginal
#' means and formating options for the ANOVA table (e.g., detailed, rounding).
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effectSize "ges" (generalized eta-squared), "es" (eta-squared), or
#' "pes" (partial eta-squared)
#' @param sphericityCorrections TRUE/FALSE (ezANOVA)
#' @param sphericityCorrectionType "GG" (default) vs. "HF" (ezANOVA)
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
#' aovRT <- aov(rt ~ Comp*Side + Error(VP/(Comp*Side)), datAggVP)
#' aovRT <- aovTable(aovRT)
#'
#'
#' @export
aovTable <- function(aovObj,
                     effectSize = "pes",
                     sphericityCorrections = TRUE,
                     sphericityCorrectionType = "GG",
                     removeSumSquares = TRUE,
                     removeIntercept = TRUE,
                     roundDigits = TRUE,
                     numDigits = 2,
                     dispAovTable = TRUE,
                     dispAovMeans = FALSE,
                     caption = NULL) {

  aov_function <- "ezANOVA"
  if (length(class(aovObj)) > 1) {
    aov_function <- "aov"
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  if (!"SSn" %in% names(aovObj$ANOVA)) {
    stop("Call ezANOVA with \"detailed = TRUE\"!")
  }

  if (aov_function == "ezANOVA" & !"aov" %in% names(aovObj)) {
      stop("Call ezANOVA with \"return_aov = TRUE\"!")
  }

  if (effectSize != "ges") {
      aovObj <- aovEffectSize(aovObj, effectSize)
  }

  if (sphericityCorrections & any(aovObj$ANOVA$DFn > 1)) {
    if(aov_function == "aov") {
      warning("Sphericity Corrections not within aov. Use ezANOVA.")
    }
    aovObj <- aovSphericityAdjustment(aovObj, sphericityCorrectionType)
  }

  if (aov_function == "ezANOVA" & removeIntercept) {
    aovObj$ANOVA <- aovObj$ANOVA[-c(1), ]
  }

  # p-value summary *** vs. ** vs *
  aovObj$ANOVA$"p<.05" <- pValueSummary(aovObj$ANOVA$p)

  if (roundDigits) {
    aovObj <- aovRoundDigits(aovObj, nsmall = numDigits)
  }

  if (removeSumSquares) {
    aovObj$ANOVA$SSn <- NULL
    aovObj$ANOVA$SSd <- NULL
  }

  if (aov_function == "ezANOVA") {
    aovObj$means <- stats::model.tables(aovObj$aov, type = "mean")
  }

  if (dispAovTable) {
      if (is.null(caption)) {
          aovDispTable(aovObj, caption = sys.call())
      } else {
          aovDispTable(aovObj, caption = caption)
      }
  }

  if (dispAovMeans) {
      if (is.null(caption)) {
          aovDispMeans(aovObj, caption = sys.call())
      } else {
          aovDispMeans(aovObj, caption = caption)
      }
  }

  aovObj$aovTable = TRUE

  return(aovObj)

}



#' @title effectsizeValueString
#'
#' @description Returns required Latex formatted string for effect size (eta squared, partial
#' eta square, generalized eta squared) = XXX for R/Sweave integration.
#' Returns values to 2 sig decimal places.
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effect The effect within the ANOVA table to return
#' @param effectSize The effect size to report ("ges" default within ezANOVA, "pes" partial eta squared)
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
#' aovRT <- aov(rt ~ Comp*Side + Error(VP/(Comp*Side)), datAggVP)
#' aovRT <- aovTable(aovRT)
#'
#' pesString <- effectsizeValueString(aovRT, "Comp")  # partial eta squared
#' pesString <- effectsizeValueString(aovRT, "Comp:Side")
#'
#' @export
effectsizeValueString <- function(aovObj, effect, effectSize = "pes"){

  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  effectSizeIdx <- which(names(aovObj$ANOVA) %in% effectSize)
  if (length(effectSizeIdx) == 0) {
    stop("Requested effect size not present in ANOVA table!")
  }
  if (effectSize == "ges") {
    effectSizeValue <- aovObj$ANOVA[, "ges"][aovObj$ANOVA$Effect == effect]
    return(paste0("$\\eta_{G}^2$ = ", effectSizeValue))
  } else if (effectSize == "pes") {
    effectSizeValue  <- aovObj$ANOVA[, "pes"][aovObj$ANOVA$Effect == effect]
    return(paste0("$\\eta_{p}^2$ = ", effectSizeValue))
  }
}



#' @title fValueString
#'
#' @description Returns required Latex formatted string for \emph{F}(df1, df2) = XXX
#' for R/Sweave integration. For example, \emph{F}(1, 23) = 3.45.
#' Returns values to 2 sig decimal places.
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
#' @param effect The effect within the ANOVA table to return
#'
#' @return character
#'
#' @examples
#'
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
#' aovRT <- aov(rt ~ Comp*Side + Error(VP/(Comp*Side)), datAggVP)
#' aovRT <- aovTable(aovRT)
#'
#' fString <- fValueString(aovRT, "Comp")
#' fString <- fValueString(aovRT, "Comp:Side")
#'
#' @export
fValueString <- function(aovObj, effect){

  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  DFn    <- aovObj$ANOVA[, "DFn"][aovObj$ANOVA$Effect == effect]
  DFd    <- aovObj$ANOVA[, "DFd"][aovObj$ANOVA$Effect == effect]
  fValue <- aovObj$ANOVA[, "F"][aovObj$ANOVA$Effect == effect]

  return(paste0("\\emph{F}", "(", DFn, ", ", DFd, ") = ", fValue))
}



#' @title meanStrAov
#'
#' @description Returns marginal means from ezANOVA object for requested effect in Latex format.
#' Assumes means added to aovObj (e.g., aovObj$means <- model.tables(aovObj$aov, type = "mean").
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
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
#' aovRT <- aov(rt ~ Comp*Side + Error(VP/(Comp*Side)), datAggVP)
#' aovRT <- aovTable(aovRT)
#'
#' meanString <- meanStrAov(aovRT, "Comp", "comp")
#' meanString <- meanStrAov(aovRT, "Comp:Side", "incomp:left")
#'
#' @export
meanStrAov <- function(aovObj, effect, level, unit = "ms", numDigits = 0) {

  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  row <- which(aovObj$ANOVA$Effect == effect) + 1
  dat <- as.data.frame.table(aovObj$means$tables[[row]], responseName = "DV")

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
#' @param ... Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
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
#' aovRT <- aov(RT ~ Comp + Error(VP/(Comp)), dat)
#' aovRT <- aovTable(aovRT)
#' printAovMeans(aovRT, digits = 0, dv = "mV")  # latex formatted
#'
#' @export
printAovMeans <- function(..., caption = "Mean", digits = 3, dv = "ms") {

  if (length(class(aovObj)) > 1) {
    if (is.null(aovObj$ANOVA)) {
      aovObj <- aovTidyTable(aovObj)
    }
  }

  aovObj <- list(...)
  for (i in seq(1:length(aovObj))) {
    if (is.null(aovObj[[i]]$means)) {
      stop("ezANOVA object does not contain marginal means!\nCall ezANOVA with \"return_aov = TRUE\"")
    }
  }
  if (!length(digits) %in% c(1, length(aovObj))) {
    stop("length digits must equal 1 or number of ezObj inputs")
  }
  if (!length(dv) %in% c(1, length(aovObj))) {
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

  for (i in 2:(length(aovObj[[1]]$means$n) + 1)) {

    tab <- as.data.frame.table(aovObj[[1]]$means$tables[[i]])
    names(tab)[ncol(tab)] <- dv[1]
    for (j in 1:length(aovObj)) {
      tab <- cbind(tab, as.data.frame.table(aovObj[[j]]$means$tables[[i]]))
      names(tab)[ncol(tab)] <- dv[j]
    }

    tab <- tab[, !duplicated(colnames(tab))]
    printTable(tab,
               caption = paste0(caption, ": ", names(aovObj[[1]]$means$tables[i])),
               digits = digits)
  }

}



#' @title statStrAov
#'
#' @description Returns Latex formatted string from ANOVA required for R/Sweave integration.
#' For example, \deqn{F(1, 20) = 8.45, p < 0.01, pes = 0.45}
#' Returns values to 2 sig decimal places and < 0.01, < 0.001 for p values.
#'
#' @param aovObj Output from aov or ezANOVA (NB. ezANOVA must be called with detailed = TRUE)
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
#' aovRT <- aov(rt ~ Comp*Side + Error(VP/(Comp*Side)), datAggVP)
#' aovRT <- aovTable(aovRT)
#'
#' aovString <- statStrAov(aovRT, "Comp")
#' aovString <- statStrAov(aovRT, "Comp:Side")
#'
#' @export
statStrAov <- function(aovObj, effect) {

  fString <- fValueString(aovObj, effect)
  pString <- pValueString(aovObj$ANOVA[, "p"][aovObj$ANOVA$Effect == effect])
  eString <- effectsizeValueString(aovObj, effect)
  sString <- sphericityValueString(aovObj, effect)

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
