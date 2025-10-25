#' Mean and confidence interval (weighted or unweighted)
#'
#' Compute the mean and a confidence interval for a numeric variable, with optional
#' survey-style weights. Returns count, Kish effective sample size, standard deviation,
#' standard error, and CI bounds. Uses a t critical by default (df = n - 1 or n_eff - 1).
#'
#' @param df A data frame.
#' @param var The numeric variable to summarize (unquoted, tidyselect-style).
#' @param wt Optional nonnegative numeric weights column (unquoted). If `NULL`, computes
#'   unweighted statistics.
#' @param ci Confidence level in (0, 1). Default is `0.95`.
#' @param dist Character string, `"t"` (default) or `"normal"`, selecting the critical
#'   distribution used for the CI.
#' @param na.rm Logical; if `TRUE`, drop `NA` values (and `NA`/nonpositive weights).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{mean} Mean of \code{var}.
#'   \item \code{sd}   (Weighted) standard deviation.
#'   \item \code{n}    Row count used (after NA/weight filtering).
#'   \item \code{n_eff} Kish effective sample size (equals \code{n} if unweighted).
#'   \item \code{se}   Standard error of the mean.
#'   \item \code{lower}, \code{upper} Confidence interval bounds.
#'   \item \code{ci}  Confidence level used.
#' }
#'
#' @details
#' `var` should refer to a single numeric column. Kish effective sample size is
#' computed as `(sum(w)^2) / sum(w^2)`. The weighted variance uses an unbiased
#' correction `neff/(neff - 1)` when `neff > 1`. With `na.rm = FALSE`, `n` counts
#' rows including `NA`s and summary statistics may return `NA`.
#'
#' @examples
#' df <- tibble::tibble(x = c(1,2,3,4,NA), w = c(1,1,2,2,1))
#' mean_ci(df, x)
#' mean_ci(df, x, wt = w)
#' mean_ci(df, x, wt = w, ci = 0.90, dist = "normal")
#'
#' @export
#' @importFrom dplyr summarise across pick
#' @importFrom tidyr unnest
#' @importFrom rlang enquo quo_is_missing as_name
#' @importFrom tibble tibble
#' @importFrom tidyselect eval_select
mean_ci <- function(df, var, wt = NULL, ci = 0.95, dist = c("t", "normal"), na.rm = TRUE) {
  stopifnot(is.numeric(ci), length(ci) == 1, is.finite(ci), ci > 0, ci < 1)
  dist <- match.arg(dist)
  
  var <- rlang::enquo(var)
  wt  <- rlang::enquo(wt)
  
  # Validate: exactly one column selected and numeric
  sel <- tidyselect::eval_select(var, df)
  if (length(sel) != 1L) stop("`var` must select exactly one numeric column.", call. = FALSE)
  if (!is.numeric(df[[sel]])) stop("`var` must refer to a numeric column.", call. = FALSE)
  
  compute_stats <- function(x, w = NULL, ci = 0.95, dist = "t") {
    if (!is.numeric(x)) stop("`var` must be numeric.", call. = FALSE)
    
    if (is.null(w)) {
      if (na.rm) x <- x[!is.na(x)]
      if (!length(x)) {
        return(tibble::tibble(mean = NA_real_, sd = NA_real_, n = 0L, n_eff = 0,
                              se = NA_real_, lower = NA_real_, upper = NA_real_, ci = ci))
      }
      N_raw <- length(x)
      m  <- mean(x)
      sd <- stats::sd(x)
      df_ <- max(N_raw - 1, 1)
      alpha <- 1 - ci
      crit <- if (dist == "t") stats::qt(1 - alpha/2, df = df_) else stats::qnorm(1 - alpha/2)
      se <- sd / sqrt(N_raw)
      return(tibble::tibble(
        mean = m, sd = sd, n = N_raw, n_eff = N_raw,
        se = se, lower = m - crit * se, upper = m + crit * se, ci = ci
      ))
    } else {
      if (!is.numeric(w)) stop("`wt` must be numeric.", call. = FALSE)
      if (length(w) != length(x)) stop("`wt` must be same length as `var`.", call. = FALSE)
      
      keep <- is.finite(w) & (w > 0)
      if (na.rm) keep <- keep & !is.na(x) & !is.na(w)
      x <- x[keep]; w <- w[keep]
      if (!length(x)) {
        return(tibble::tibble(mean = NA_real_, sd = NA_real_, n = 0L, n_eff = 0,
                              se = NA_real_, lower = NA_real_, upper = NA_real_, ci = ci))
      }
      w_sum <- sum(w)
      if (!is.finite(w_sum) || w_sum <= 0) stop("Sum of weights must be positive.", call. = FALSE)
      
      N_raw <- length(x)
      w_norm <- w / w_sum
      m <- sum(w_norm * x)
      
      var_w_pop <- sum(w_norm * (x - m)^2)
      neff <- (w_sum^2) / sum(w^2)
      var_w_unb <- if (neff > 1) var_w_pop * (neff / (neff - 1)) else var_w_pop
      
      sd_w <- sqrt(var_w_unb)
      df_ <- max(neff - 1, 1)
      alpha <- 1 - ci
      crit <- if (dist == "t") stats::qt(1 - alpha/2, df = df_) else stats::qnorm(1 - alpha/2)
      se <- sqrt(var_w_unb / neff)
      
      return(tibble::tibble(
        mean = m, sd = sd_w, n = as.integer(N_raw), n_eff = as.numeric(neff),
        se = se, lower = m - crit * se, upper = m + crit * se, ci = ci
      ))
    }
  }
  
  dplyr::summarise(
    df,
    dplyr::across(
      {{ var }},
      ~ {
        if (rlang::quo_is_missing(wt)) {
          compute_stats(.x, w = NULL, ci = ci, dist = dist)
        } else {
          wcol <- dplyr::pick(!!wt)[[1]]
          compute_stats(.x, w = wcol, ci = ci, dist = dist)
        }
      },
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |>
    tidyr::unnest(cols = dplyr::all_of(rlang::as_name(var)))
}
