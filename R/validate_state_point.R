# ------------------------------------------------------------------------------
# State validation: continuous-ish variables against point/mean summaries
# ------------------------------------------------------------------------------

.obs_state_moments <- function(obs, var, times, measured_only) {
  if (is.null(obs$state) || is.null(obs$state[[var]])) {
    stop(sprintf("obs does not contain gridded state for var '%s'.", var), call. = FALSE)
  }
  X <- obs$state[[var]]
  if (!is.matrix(X)) stop("obs$state[[var]] must be a matrix.", call. = FALSE)

  t_idx <- match(times, obs$times)
  denom <- .get_state_masks(obs, var = var, measured_only = measured_only)

  out <- vector("list", length(t_idx))
  for (jj in seq_along(t_idx)) {
    k <- t_idx[jj]
    elig <- as.logical(denom[, k])
    vals <- X[elig, k]
    n <- sum(!is.na(vals))
    mu <- if (n == 0L) NA_real_ else mean(vals, na.rm = TRUE)
    sdv <- if (n <= 1L) NA_real_ else stats::sd(vals, na.rm = TRUE)
    out[[jj]] <- data.frame(time = times[jj], n = n, mean = mu, sd = sdv)
  }
  do.call(rbind, out)
}

validate_state_point <- function(
  pred,
  obs,
  var,
  times = NULL,
  start_time = NULL,
  measured_only = FALSE
) {
  if (!inherits(obs, "flux_obs_grid")) stop("obs must be a flux_obs_grid.", call. = FALSE)
  if (!is.character(var) || length(var) != 1L || !nzchar(var)) stop("var must be a non-empty character scalar.", call. = FALSE)

  # Allow flux_forecast input by adapting to a lightweight summary.
  if (inherits(pred, "flux_forecast")) {
    x <- as_state_point(pred, var = var, times = times, start_time = start_time)
  } else if (inherits(pred, "flux_state_point")) {
    x <- pred
  } else if (is.list(pred) && !is.null(pred$spec) && !is.null(pred$result)) {
    x <- pred
  } else {
    stop("pred must be a flux_state_point, flux_forecast, or flux_state_point-like list.", call. = FALSE)
  }

  if (is.null(times)) times <- x$spec$times
  if (is.null(start_time)) start_time <- x$spec$start_time
  times <- sort(unique(as.numeric(times)))
  if (!all(times %in% obs$times)) stop("times must be a subset of obs$times.", call. = FALSE)
  if (!all(times %in% x$spec$times)) stop("times must be a subset of pred$spec$times.", call. = FALSE)

  start_time <- as.numeric(start_time)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be included in times.", call. = FALSE)

  observed <- .obs_state_moments(obs, var = var, times = times, measured_only = measured_only)

  pr <- x$result
  if (!all(c("time", "mean") %in% names(pr))) stop("pred$result must include time and mean.", call. = FALSE)
  pr <- pr[pr$time %in% times, , drop = FALSE]
  pr$time <- as.numeric(pr$time)
  pr <- pr[order(pr$time), , drop = FALSE]

  # Metrics: compare predicted mean to observed mean at each time.
  m <- merge(
    observed[, c("time", "n", "mean"), drop = FALSE],
    pr[, c("time", "mean"), drop = FALSE],
    by = "time",
    suffixes = c("_obs", "_pred"),
    all.x = TRUE,
    all.y = FALSE
  )
  m$err <- m$mean_pred - m$mean_obs
  m$abs_err <- abs(m$err)
  m$sq_err <- m$err * m$err
  metrics <- m[, c("time", "n", "mean_obs", "mean_pred", "err", "abs_err", "sq_err"), drop = FALSE]

  list(
    predicted = pr,
    observed = observed,
    metrics = metrics,
    meta = list(var = var, times = times, start_time = start_time, measured_only = measured_only)
  )
}
