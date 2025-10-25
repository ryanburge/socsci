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


mean_ci <- function(df, var, wt = NULL, ci = 0.95, dist = c("t", "normal"), na.rm = TRUE) {
  dist <- match.arg(dist)
  var <- rlang::enquo(var)
  wt  <- rlang::enquo(wt)
  
  compute_stats <- function(x, w = NULL, ci = 0.95, dist = "t") {
    if (!is.numeric(x)) stop("`var` must be numeric.", call. = FALSE)
    
    if (is.null(w)) {
      if (na.rm) x <- x[!is.na(x)]
      if (!length(x)) {
        return(dplyr::tibble(mean = NA_real_, sd = NA_real_, n = 0L, n_eff = 0,
                             se = NA_real_, lower = NA_real_, upper = NA_real_, ci = ci))
      }
      N_raw <- length(x)
      m  <- mean(x)
      sd <- stats::sd(x)
      df_ <- max(N_raw - 1, 1)
      alpha <- 1 - ci
      crit <- if (dist == "t") stats::qt(1 - alpha/2, df = df_) else stats::qnorm(1 - alpha/2)
      se <- sd / sqrt(N_raw)
      return(dplyr::tibble(
        mean = round(m, 3), sd = sd, n = N_raw, n_eff = N_raw,
        se = se, lower = m - crit*se, upper = m + crit*se, ci = ci
      ))
    } else {
      if (!is.numeric(w)) stop("`wt` must be numeric.", call. = FALSE)
      if (length(w) != length(x)) stop("`wt` must be same length as `var`.", call. = FALSE)
      
      keep <- !is.na(x) & !is.na(w) & is.finite(w) & (w > 0)
      x <- x[keep]; w <- w[keep]
      if (!length(x)) {
        return(dplyr::tibble(mean = NA_real_, sd = NA_real_, n = 0L, n_eff = 0,
                             se = NA_real_, lower = NA_real_, upper = NA_real_, ci = ci))
      }
      N_raw <- length(x)
      w_sum <- sum(w)
      w_norm <- w / w_sum
      m <- sum(w_norm * x)
      
      # weighted variance + Kish neff
      var_w_pop <- sum(w_norm * (x - m)^2)
      neff <- (w_sum^2) / sum(w^2)
      var_w_unb <- if (neff > 1) var_w_pop * (neff / (neff - 1)) else var_w_pop
      
      sd_w <- sqrt(var_w_unb)
      df_ <- max(neff - 1, 1)
      alpha <- 1 - ci
      crit <- if (dist == "t") stats::qt(1 - alpha/2, df = df_) else stats::qnorm(1 - alpha/2)
      se <- sqrt(var_w_unb / neff)
      
      return(dplyr::tibble(
        mean = round(m, 3), sd = sd_w, n = as.integer(N_raw), n_eff = as.numeric(neff),
        se = se, lower = m - crit*se, upper = m + crit*se, ci = ci
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