# Internal sphericity functions
# These compute Mauchly's test for sphericity and GG/HF epsilon corrections
# using base R linear algebra (no external dependencies required).

# Build a (k x k-1) contrast matrix.
# Rows 1..k-1 have a 1 on the diagonal, row k has -1 in every column.
.contrastMatrix <- function(k) {
  P <- matrix(0, nrow = k, ncol = k - 1)
  for (i in 1:(k - 1)) {
    P[i, i] <- 1
    P[k, i] <- -1
  }
  return(P)
}

# Build a subject x condition matrix of cell means from long-format data.
# Returns an N x k numeric matrix (N subjects, k conditions).
.subjectConditionMatrix <- function(data, dv, id, factors) {
  subjects  <- sort(unique(data[[id]]))
  n_subj    <- length(subjects)

  # Build condition grid (all factor-level combinations)
  # Use rev() so first factor varies slowest, matching kronecker() convention
  level_list <- lapply(factors, function(f) sort(unique(data[[f]])))
  cond_grid  <- expand.grid(rev(level_list))
  cond_grid  <- cond_grid[, rev(seq_len(ncol(cond_grid))), drop = FALSE]
  names(cond_grid) <- factors
  n_cond <- nrow(cond_grid)

  Y <- matrix(0, nrow = n_subj, ncol = n_cond)

  subj_map <- setNames(seq_along(subjects), as.character(subjects))

  for (j in 1:n_cond) {
    idx <- rep(TRUE, nrow(data))
    for (f in factors) {
      idx <- idx & (data[[f]] == cond_grid[j, f])
    }
    for (i in 1:n_subj) {
      rows <- idx & (data[[id]] == subjects[i])
      vals <- data[[dv]][rows]
      Y[i, j] <- mean(vals)
    }
  }

  return(Y)
}

# Compute the SSPE (sum of squares and products of errors) matrix.
.computeSSPE <- function(Y) {
  col_means   <- colMeans(Y)
  Y_centered  <- sweep(Y, 2, col_means)
  return(t(Y_centered) %*% Y_centered)
}


# Build contrast matrix for a (possibly interaction) effect.
# For single factors, returns .contrastMatrix(k).
# For interactions, returns the Kronecker product of per-factor contrast matrices.
.buildContrastMatrixForEffect <- function(factors, data) {
  n_per_factor <- sapply(factors, function(f) length(unique(data[[f]])))

  if (length(factors) == 1) {
    return(.contrastMatrix(n_per_factor))
  }

  P <- .contrastMatrix(n_per_factor[1])
  for (i in 2:length(n_per_factor)) {
    P <- kronecker(P, .contrastMatrix(n_per_factor[i]))
  }
  return(P)
}


# Compute the sphericity components (U matrix, n_contrasts, df, n_conditions)
# for a given set of factors.
# Returns NULL if < 3 cells (sphericity is trivially satisfied for 2 levels).
.getSphericityComponents <- function(data, dv, id, factors) {
  n_cells <- prod(sapply(factors, function(f) length(unique(data[[f]]))))
  if (n_cells < 3) return(NULL)

  Y    <- .subjectConditionMatrix(data, dv, id, factors)
  SSPE <- .computeSSPE(Y)
  P    <- .buildContrastMatrixForEffect(factors, data)

  transformed_sspe <- t(P) %*% SSPE %*% P
  p_transpose_p    <- t(P) %*% P
  u_matrix         <- solve(p_transpose_p, transformed_sspe)

  n_contrasts  <- ncol(transformed_sspe)
  n_conditions <- nrow(P)
  df           <- length(unique(data[[id]])) - 1

  return(list(
    u_matrix     = u_matrix,
    n_contrasts  = n_contrasts,
    df           = df,
    n_conditions = n_conditions
  ))
}


# Mauchly's test for sphericity.
# Returns list(W, p).
# For effects with < 3 levels/cells, returns W=1, p=1 (sphericity trivially satisfied).
.mauchlyTest <- function(data, dv, id, factors) {
  components <- .getSphericityComponents(data, dv, id, factors)
  if (is.null(components)) return(list(W = 1.0, p = 1.0))

  u_matrix     <- components$u_matrix
  n_contrasts  <- components$n_contrasts
  df           <- components$df
  n_conditions <- components$n_conditions

  if (n_contrasts < 2) return(list(W = 1.0, p = 1.0))

  # Compute W statistic
  det_u   <- det(u_matrix)
  trace_u <- sum(diag(u_matrix))
  logW    <- log(det_u) - n_contrasts * log(trace_u / n_contrasts)
  W       <- exp(logW)

  # Bartlett correction (from R's stats:::mauchly.test.SSD)
  rho <- 1.0 - (2 * n_contrasts^2 + n_contrasts + 2) / (6 * n_contrasts * df)

  # Second-order correction factor
  w2 <- (n_contrasts + 2) * (n_contrasts - 1) * (n_contrasts - 2) *
    (2 * n_contrasts^3 + 6 * n_contrasts^2 + 3 * n_conditions + 2) /
    (288 * (df * n_contrasts * rho)^2)

  # Test statistic
  z <- -df * rho * logW

  # Degrees of freedom for chi-square test
  f <- n_contrasts * (n_contrasts + 1) / 2 - 1
  if (f <= 0) return(list(W = 1.0, p = 1.0))

  # p-value with second-order correction
  Pr1 <- 1.0 - stats::pchisq(z, df = f)
  Pr2 <- 1.0 - stats::pchisq(z, df = f + 4)
  p   <- Pr1 + w2 * (Pr2 - Pr1)
  p   <- max(0.0, min(1.0, p))

  return(list(W = W, p = p))
}


# Compute GG or HF epsilon from the U matrix.
.computeEpsilon <- function(u_matrix, n_contrasts, df, type = "GG") {
  trace_u            <- sum(diag(u_matrix))
  sum_squared        <- sum(u_matrix * t(u_matrix))
  gg_epsilon         <- trace_u^2 / (n_contrasts * sum_squared)

  if (type == "GG") return(gg_epsilon)

  # Huynh-Feldt
  n_subjects  <- df + 1
  numerator   <- n_subjects * n_contrasts * gg_epsilon - 2
  denominator <- n_contrasts * (n_subjects - 1 - n_contrasts * gg_epsilon)
  hf_epsilon  <- numerator / denominator
  return(min(1.0, hf_epsilon))
}


# Top-level function: compute sphericity data for all effects in an aov object.
#
# Takes the raw data and the ANOVA table (from aovTidyTable), and returns
# a list with two components:
#   $"Mauchly's Test for Sphericity" — data.frame with Effect, W, p, p<.05
#   $"Sphericity Corrections"        — data.frame with Effect, GGe, p[GG], HFe, p[HF]
#
# Only effects with DFn > 1 are included (DFn == 1 means 2 levels = no sphericity issue).
.computeSphericity <- function(aovTable, data) {

  effects <- aovTable$ANOVA

  # find effects with DFn > 1
  sph_rows <- which(effects$DFn > 1)
  if (length(sph_rows) == 0) return(NULL)

  # Identify columns
  dv_name <- NULL
  id_name <- NULL

  # Try to infer dv and id from the aov model's terms/formula
  effect_names <- effects$Effect
  all_factors  <- unique(unlist(strsplit(effect_names, ":")))

  # psychReport: VP is the subject ID
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  factor_cols  <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x))]

  # DV is the numeric column that is NOT a factor in the ANOVA
  dv_candidates <- setdiff(numeric_cols, all_factors)
  if (length(dv_candidates) == 0) {
    # all numeric — try to find DV by elimination
    dv_candidates <- setdiff(numeric_cols, c(all_factors, factor_cols))
  }

  # ID is the factor column not in the ANOVA effects
  id_candidates <- setdiff(factor_cols, all_factors)

  if (length(dv_candidates) == 0 || length(id_candidates) == 0) return(NULL)

  dv_name <- dv_candidates[1]
  id_name <- id_candidates[1]

  # Build Mauchly's test table
  mauchly_df <- data.frame(
    Effect  = character(0),
    W       = numeric(0),
    p       = numeric(0),
    stringsAsFactors = FALSE
  )

  # Build sphericity corrections table
  corrections_df <- data.frame(
    Effect = character(0),
    GGe    = numeric(0),
    `p[GG]`= numeric(0),
    HFe    = numeric(0),
    `p[HF]`= numeric(0),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (row_idx in sph_rows) {
    effect   <- effects$Effect[row_idx]
    factors  <- strsplit(effect, ":")[[1]]

    # Mauchly's test
    mauchly  <- .mauchlyTest(data, dv_name, id_name, factors)

    mauchly_df <- rbind(mauchly_df, data.frame(
      Effect = effect,
      W      = mauchly$W,
      p      = mauchly$p,
      stringsAsFactors = FALSE
    ))

    # Epsilon (GG and HF)
    components <- .getSphericityComponents(data, dv_name, id_name, factors)
    if (is.null(components)) {
      gg_eps <- 1.0
      hf_eps <- 1.0
    } else {
      gg_eps <- .computeEpsilon(components$u_matrix, components$n_contrasts,
                                components$df, "GG")
      hf_eps <- .computeEpsilon(components$u_matrix, components$n_contrasts,
                                components$df, "HF")
    }

    # Corrected p-values
    DFn <- effects$DFn[row_idx]
    DFd <- effects$DFd[row_idx]
    Fval <- effects$F[row_idx]

    p_gg <- 1.0 - stats::pf(Fval, DFn * gg_eps, DFd * gg_eps)
    p_hf <- 1.0 - stats::pf(Fval, DFn * hf_eps, DFd * hf_eps)

    corrections_df <- rbind(corrections_df, data.frame(
      Effect  = effect,
      GGe     = gg_eps,
      `p[GG]` = p_gg,
      HFe     = hf_eps,
      `p[HF]` = p_hf,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  # Add significance column to Mauchly's test
  mauchly_df$"p<.05" <- ifelse(mauchly_df$p < 0.05, "*", "")

  # Set row names to match effect names
  rownames(mauchly_df)      <- mauchly_df$Effect
  rownames(corrections_df)  <- corrections_df$Effect

  return(list(
    "Mauchly's Test for Sphericity" = mauchly_df,
    "Sphericity Corrections"        = corrections_df
  ))
}
