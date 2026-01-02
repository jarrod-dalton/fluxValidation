# ------------------------------------------------------------------------------
# Event validation (MVP): observed cumulative incidence vs predicted risk curves
# ------------------------------------------------------------------------------

validate_event_risk <- function(
  pred,
  obs,
  event,
  start_time = NULL,
  times = NULL,
  obs_mode = c("policy", "interval")
) {
  obs_mode <- match.arg(obs_mode)
  if (!inherits(obs, "ps_obs_grid")) stop("obs must be a ps_obs_grid.", call. = FALSE)
  if (!is.character(event) || length(event) != 1L || !nzchar(event)) stop("event must be a non-empty character scalar.", call. = FALSE)

  # Predicted curve input can be a ps_risk (preferred) or a ps_forecast (adapted).
  x <- pred
  if (inherits(pred, "ps_risk")) {
    if (is.null(times)) times <- pred$spec$times
    if (is.null(start_time)) start_time <- pred$spec$start_time
  } else if (inherits(pred, "ps_forecast")) {
    if (is.null(times)) times <- pred$times
    if (is.null(start_time)) start_time <- pred$time0
  } else {
    stop("pred must be a ps_forecast or ps_risk.", call. = FALSE)
  }

  times <- sort(unique(as.numeric(times)))
  if (length(times) < 1L) stop("times must be a non-empty numeric vector subset of forecast$times.", call. = FALSE)
  pred_times <- if (inherits(pred, "ps_risk")) pred$spec$times else pred$times
  if (!all(times %in% pred_times)) stop("times must be a subset of predicted object's times.", call. = FALSE)

  start_time <- as.numeric(start_time)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% pred_times) stop("start_time must be one of predicted object's times.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be included in times.", call. = FALSE)

  # predicted curve (coerce ps_forecast -> ps_risk)
  if (inherits(pred, "ps_risk")) {
    ev <- pred$spec$event
    if (is.null(ev) || !(event %in% ev)) stop("ps_risk does not contain requested event.", call. = FALSE)
    predicted <- pred
  } else {
    predicted <- as_ps_risk(pred, event = event, times = times, start_time = start_time)
  }

  # observed curve
  observed <- compute_obs_risk(obs, event = event, times = times, start_time = start_time, mode = obs_mode)

  # Comparison table (pooled curves only). If predicted has grouping columns (by != 'run'),
  # return per-group comparison.
  pr_res <- predicted$result
  # Identify potential group cols
  group_cols <- setdiff(names(pr_res), c("time", "n_eligible", "n_events", "risk"))
  if (length(group_cols) == 0) {
    cmp <- merge(
      pr_res[, c("time", "n_eligible", "n_events", "risk"), drop = FALSE],
      observed[, c("time", "n_eligible", "n_events", "risk"), drop = FALSE],
      by = "time",
      suffixes = c("_pred", "_obs"),
      all.x = TRUE,
      all.y = FALSE
    )
    cmp$err <- cmp$risk_pred - cmp$risk_obs
    cmp$abs_err <- abs(cmp$err)
  } else {
    # replicate observed across groups
    obs_map <- observed[, c("time", "risk"), drop = FALSE]
    cmp <- merge(pr_res, obs_map, by = "time", all.x = TRUE, suffixes = c("_pred", "_obs"))
    # merge() will create risk_pred (from pr_res) and risk_obs (from obs_map)
    cmp$err <- cmp$risk_pred - cmp$risk_obs
    cmp$abs_err <- abs(cmp$err)
  }

  list(
    predicted = predicted,
    observed = observed,
    comparison = cmp,
    meta = list(event = event, start_time = start_time, times = times, obs_mode = obs_mode)
  )
}

