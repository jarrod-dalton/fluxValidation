# ------------------------------------------------------------------------------
# State validation: categorical/binary variables against probability summaries
# ------------------------------------------------------------------------------

.get_state_masks <- function(obs, var, measured_only = FALSE) {
  if (is.null(obs$alive_mask) || is.null(obs$followup_defined)) {
    stop("obs must contain alive_mask and followup_defined.", call. = FALSE)
  }
  model_defined <- NULL
  if (!is.null(obs$model_defined) && is.list(obs$model_defined) && !is.null(obs$model_defined[[var]])) {
    model_defined <- obs$model_defined[[var]]
  }
  measured <- NULL
  if (measured_only) {
    if (is.null(obs$measured) || !is.list(obs$measured) || is.null(obs$measured[[var]])) {
      stop("measured_only=TRUE requires obs$measured[[var]].", call. = FALSE)
    }
    measured <- obs$measured[[var]]
  }
  make_denom_mask(
    alive_mask = obs$alive_mask,
    followup_defined = obs$followup_defined,
    model_defined = model_defined,
    measured = measured,
    measured_only = measured_only
  )
}

.obs_state_level_props <- function(obs, var, levels, times, start_time, measured_only) {
  if (is.null(obs$state) || is.null(obs$state[[var]])) {
    stop(sprintf("obs does not contain gridded state for var '%s'.", var), call. = FALSE)
  }
  X <- obs$state[[var]]  # [patients x times]
  if (!is.matrix(X)) stop("obs$state[[var]] must be a matrix.", call. = FALSE)

  t_idx <- match(times, obs$times)
  denom <- .get_state_masks(obs, var = var, measured_only = measured_only)

  out <- vector("list", length(t_idx))
  for (jj in seq_along(t_idx)) {
    k <- t_idx[jj]
    elig <- as.logical(denom[, k])
    vals <- X[, k]
    # Only count those in denominator; NA values remain and are treated as missing values.
    vals <- vals[elig]
    n <- sum(!is.na(vals))
    tab <- table(factor(vals, levels = levels), useNA = "no")
    prop <- if (n == 0L) rep(NA_real_, length(levels)) else as.numeric(tab) / n
    out[[jj]] <- data.frame(time = times[jj], level = levels, n = n, prop = prop)
  }
  do.call(rbind, out)
}

validate_state_prob <- function(
  pred,
  obs,
  var,
  times = NULL,
  start_time = NULL,
  measured_only = FALSE
) {
  if (!inherits(obs, "ps_obs_grid")) stop("obs must be a ps_obs_grid.", call. = FALSE)
  if (!is.character(var) || length(var) != 1L || !nzchar(var)) stop("var must be a non-empty character scalar.", call. = FALSE)

  # Allow ps_forecast input by adapting to a lightweight summary.
  if (inherits(pred, "ps_forecast")) {
    x <- as_state_prob(pred, var = var, times = times, start_time = start_time)
  } else if (inherits(pred, "ps_state_prob")) {
    x <- pred
  } else if (is.list(pred) && !is.null(pred$spec) && !is.null(pred$result)) {
    x <- pred
  } else {
    stop("pred must be a ps_state_prob, ps_forecast, or ps_state_prob-like list.", call. = FALSE)
  }

  if (is.null(times)) times <- x$spec$times
  if (is.null(start_time)) start_time <- x$spec$start_time
  times <- sort(unique(as.numeric(times)))
  if (!all(times %in% obs$times)) stop("times must be a subset of obs$times.", call. = FALSE)
  if (!all(times %in% x$spec$times)) stop("times must be a subset of pred$spec$times.", call. = FALSE)

  start_time <- as.numeric(start_time)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be included in times.", call. = FALSE)

  lev <- x$spec$levels
  if (is.null(lev) || !is.character(lev) || length(lev) < 2L) {
    stop("pred$spec$levels must be a character vector of length>=2.", call. = FALSE)
  }

  observed <- .obs_state_level_props(obs, var = var, levels = lev, times = times, start_time = start_time, measured_only = measured_only)

  # Predicted: filter to requested times and var levels
  pr <- x$result
  pr <- pr[pr$time %in% times, , drop = FALSE]
  pr$time <- as.numeric(pr$time)
  pr <- pr[order(pr$time, pr$level), , drop = FALSE]

  # Basic proper scoring rules (population-level): log loss and multiclass Brier.
  # These use observed level proportions at each time, weighted by n.
  obs_wide <- split(observed, observed$time)
  pr_wide <- split(pr, pr$time)
  met_rows <- vector("list", length(times))
  for (ii in seq_along(times)) {
    t <- times[[ii]]
    odf <- obs_wide[[as.character(t)]]
    pdf <- pr_wide[[as.character(t)]]
    if (is.null(odf) || is.null(pdf)) {
      met_rows[[ii]] <- data.frame(time = t, n = NA_integer_, log_loss = NA_real_, brier = NA_real_)
      next
    }
    # align by levels
    odf <- odf[match(lev, odf$level), , drop = FALSE]
    pdf <- pdf[match(lev, pdf$level), , drop = FALSE]
    p_obs <- as.numeric(odf$prop)
    p_pred <- as.numeric(pdf$prob)
    n <- as.integer(odf$n[1])
    # log loss: -sum p_obs * log(p_pred)
    ll <- NA_real_
    if (!all(is.na(p_obs)) && !all(is.na(p_pred))) {
      # Treat zeros carefully: if p_pred==0 where p_obs>0 -> Inf.
      ll <- 0
      for (j in seq_along(lev)) {
        if (is.na(p_obs[[j]]) || p_obs[[j]] == 0) next
        if (is.na(p_pred[[j]]) || p_pred[[j]] <= 0) {
          ll <- Inf
          break
        }
        ll <- ll - p_obs[[j]] * log(p_pred[[j]])
      }
    }
    # multiclass Brier score
    br <- NA_real_
    if (!all(is.na(p_obs)) && !all(is.na(p_pred))) {
      dif <- p_pred - p_obs
      br <- sum(dif * dif, na.rm = TRUE)
    }
    met_rows[[ii]] <- data.frame(time = t, n = n, log_loss = ll, brier = br)
  }
  metrics <- do.call(rbind, met_rows)

  list(
    predicted = pr,
    observed = observed,
    metrics = metrics,
    meta = list(var = var, times = times, start_time = start_time, measured_only = measured_only)
  )
}
