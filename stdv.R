custom_sd <- function(x, na.rm = FALSE, use) {

  # ── 1. Resolve 'use' argument (mirrors cov() logic) ──────────────────────────
  if (missing(use))
    use <- if (na.rm) "na.or.complete" else "everything"

  na.method <- pmatch(use, c("all.obs", "complete.obs", "everything", "na.or.complete"))

  if (is.na(na.method))
    stop("invalid 'use' argument. Choose from: 'all.obs', 'complete.obs', 'everything', 'na.or.complete'")


  # ── 2. Block illegal types ────────────────────────────────────────────────────
  if (is.list(x) && !is.data.frame(x))
    stop("input cannot be a list. Provide a numeric vector, matrix, or data frame.")

  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt"))
    stop("input cannot be a Date/time object. Provide numeric data.")

  if (is.factor(x))
    stop("input cannot be a factor. Provide numeric data.")


  # ── 3. Core SD calculator for a single vector ─────────────────────────────────
  .compute_sd <- function(vec) {

    # Return NA for non-numeric columns (e.g., character)
    if (!is.numeric(vec)) return(NA_real_)

    # Block factor/date sneaking in through data frames
    if (is.factor(vec) || inherits(vec, "Date")) return(NA_real_)

    # Handle 'use' / NA strategy
    if (na.method == 1) {           # "all.obs" → error if any NA
      if (anyNA(vec))
        stop("missing values present and use = 'all.obs'")

    } else if (na.method == 2) {    # "complete.obs" → silently remove NAs
      vec <- vec[!is.na(vec)]

    } else if (na.method == 3) {    # "everything" → NA propagates
      if (anyNA(vec)) return(NA_real_)

    } else if (na.method == 4) {    # "na.or.complete" → remove NAs, return NA if all gone
      vec <- vec[!is.na(vec)]
      if (length(vec) == 0) return(NA_real_)
    }

    # Need at least 2 observations to compute SD
    if (length(vec) < 2) {
      warning("fewer than 2 complete observations — SD is undefined, returning NA")
      return(NA_real_)
    }

    # ── Core SD formula: sqrt( Σ(xi - x̄)² / (n-1) ) ──
    n    <- length(vec)
    mean_x <- sum(vec) / n
    sd_val <- sqrt(sum((vec - mean_x)^2) / (n - 1))

    return(sd_val)
  }


  # ── 4. Data Frame → column-wise result ───────────────────────────────────────
  if (is.data.frame(x)) {
    results <- sapply(x, function(col) {

      # Explicitly catch illegal column types
      if (is.factor(col))
        return(NA_real_)          # factor column → NA

      if (inherits(col, "Date") || inherits(col, "POSIXct") || inherits(col, "POSIXlt"))
        return(NA_real_)          # date column  → NA

      if (!is.numeric(col))
        return(NA_real_)          # character etc → NA

      .compute_sd(col)
    })

    return(results)
  }


  # ── 5. Matrix → column-wise result ───────────────────────────────────────────
  if (is.matrix(x)) {
    if (!is.numeric(x))
      stop("matrix must be numeric. Non-numeric matrices are not supported.")

    results <- apply(x, 2, .compute_sd)   # 2 = column-wise

    # Preserve column names
    if (!is.null(colnames(x)))
      names(results) <- colnames(x)

    return(results)
  }


  # ── 6. Plain vector ───────────────────────────────────────────────────────────
  if (!is.atomic(x))
    stop("input must be atomic (numeric vector, matrix, or data frame).")

  return(.compute_sd(x))
}


