# ------------------------------------------------------------------------------
# Event validation (MVP): observed cumulative incidence vs predicted risk curves
# ------------------------------------------------------------------------------

#' Validate event risk curves against observed gridded data
#'
#' Compare predicted cumulative event risk curves (from [fluxForecast::event_prob()])
#' to observed cumulative incidence computed from a `flux_obs_grid`.
#'
#' @param pred A `flux_forecast` or `flux_event_prob` (preferred) object
#'   (from fluxForecast).
#' @param obs A `flux_obs_grid` object (from `build_obs_grid()`).
#' @param event Character scalar event type to validate.
#' @param start_time Optional scalar start time defining the eligible cohort.
#'   Must be one of `pred$times`. Defaults to `pred$start_time` for
#'   `flux_event_prob` or `pred$time0` for `flux_forecast`.
#' @param times Optional numeric subset of `pred$times` at which to evaluate risk.
#'   Defaults to all.
#' @param obs_mode Observed risk estimand. `"fixed_cohort"` fixes the denominator at
#'   `start_time` (Forecast-compatible). `"risk_set"` uses interval-specific
#'   risk sets via `at_risk`.
#'
#' @return A list with components `predicted`, `observed`, and `meta`.
#' @details
#' Observed risk curves are computed via `compute_obs_risk()` on the same `times` grid.
#' The default `obs_mode = "fixed_cohort"` matches the fixed-cohort estimand used by
#' [fluxForecast::event_prob()].
#' @export
validate_event_risk <- function(
  pred,
  obs,
  event,
  start_time = NULL,
  times = NULL,
  obs_mode = c("fixed_cohort", "risk_set")
) {
  obs_mode <- match.arg(obs_mode)
  if (!inherits(obs, "flux_obs_grid")) stop("obs must be a flux_obs_grid.", call. = FALSE)
  if (!is.character(event) || length(event) != 1L || !nzchar(event)) stop("event must be a non-empty character scalar.", call. = FALSE)

  # Predicted curve input can be a flux_event_prob (preferred) or a flux_forecast (adapted).
  x <- pred
  if (inherits(pred, "flux_event_prob")) {
    if (is.null(times)) times <- pred$spec$times
    if (is.null(start_time)) start_time <- pred$spec$start_time
  } else if (inherits(pred, "flux_forecast")) {
    if (is.null(times)) times <- pred$times
    if (is.null(start_time)) start_time <- pred$time0
  } else {
    stop("pred must be a flux_forecast or flux_event_prob.", call. = FALSE)
  }

  times <- sort(unique(as.numeric(times)))
  if (length(times) < 1L) stop("times must be a non-empty numeric vector subset of forecast$times.", call. = FALSE)
  pred_times <- if (inherits(pred, "flux_event_prob")) pred$spec$times else pred$times
  if (!all(times %in% pred_times)) stop("times must be a subset of predicted object's times.", call. = FALSE)

  start_time <- as.numeric(start_time)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% pred_times) stop("start_time must be one of predicted object's times.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be included in times.", call. = FALSE)

  # predicted curve (coerce flux_forecast -> flux_event_prob)
  if (inherits(pred, "flux_event_prob")) {
    ev <- pred$spec$event
    if (is.null(ev) || !(event %in% ev)) stop("flux_event_prob does not contain requested event.", call. = FALSE)
    predicted <- pred
  } else {
    predicted <- as_event_prob(pred, event = event, times = times, start_time = start_time)
  }

  # observed curve
  observed <- compute_obs_risk(obs, event = event, times = times, start_time = start_time, mode = obs_mode)

  # Comparison table (pooled curves only). If predicted has grouping columns (by != 'run'),
  # return per-group comparison.
  pr_res <- predicted$result
  # Identify potential group cols
  if (!("event_prob" %in% names(pr_res)) && ("risk" %in% names(pr_res))) pr_res$event_prob <- pr_res$risk
  group_cols <- setdiff(names(pr_res), c("time", "n_eligible", "n_events", "event_prob", "risk"))
  if (length(group_cols) == 0) {
    cmp <- merge(
      pr_res[, c("time", "n_eligible", "n_events", "event_prob"), drop = FALSE],
      observed[, c("time", "n_eligible", "n_events", "risk"), drop = FALSE],
      by = "time",
      suffixes = c("_pred", "_obs"),
      all.x = TRUE,
      all.y = FALSE
    )
    names(cmp)[names(cmp) == "risk"] <- "risk_obs"
    cmp$err <- cmp$event_prob - cmp$risk_obs
    cmp$abs_err <- abs(cmp$err)
  } else {
    # replicate observed across groups
    obs_map <- observed[, c("time", "risk"), drop = FALSE]
    cmp <- merge(pr_res, obs_map, by = "time", all.x = TRUE, suffixes = c("", "_obs"))
    names(cmp)[names(cmp) == "risk"] <- "risk_obs"
    cmp$err <- cmp$event_prob - cmp$risk_obs
    cmp$abs_err <- abs(cmp$err)
  }

  list(
    predicted = predicted,
    observed = observed,
    comparison = cmp,
    meta = list(event = event, start_time = start_time, times = times, obs_mode = obs_mode)
  )
}
