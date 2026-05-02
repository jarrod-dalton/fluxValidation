# ------------------------------------------------------------------------------
# Observed event risk estimators on a forecast grid
#
# Two estimands are supported:
#
# 1) policy (Forecast-compatible): fix the denominator at `t_ref` (start_time)
#    and keep those entities in the denominator at all subsequent times,
#    regardless of follow-up truncation. Events are counted when observed.
#
# 2) interval: interval-specific risk-set denominators using at_risk_e[k,i].
#
# Both estimands rely on explicit masks, not NA values.
# ------------------------------------------------------------------------------

#' Compute observed event risk curves on a forecast grid
#'
#' Compute observed event risk curves from a `flux_obs_grid`. This is a low-level
#' helper used by `validate_event_risk()`.
#'
#' @param obs A `flux_obs_grid`.
#' @param event Character scalar event type.
#' @param times Optional numeric subset of `obs$times`. Defaults to all.
#' @param start_time Scalar reference time in `obs$times` at which to define the
#'   eligible cohort.
#' @param mode `"policy"` fixes the denominator at `start_time` (Forecast-compatible).
#'   `"interval"` uses interval-specific risk sets via `obs$at_risk`.
#' @param measured_only Reserved for future use; currently ignored for events.
#'
#' @return A data.frame with columns `time`, `n_eligible`, `n_events`, and `risk`.
#' @export
compute_obs_risk <- function(
  obs,
  event,
  times = NULL,
  start_time,
  mode = c("policy", "interval"),
  measured_only = FALSE
) {
  mode <- match.arg(mode)
  if (!inherits(obs, "flux_obs_grid")) stop("obs must be a flux_obs_grid.", call. = FALSE)
  if (!is.character(event) || length(event) != 1L || !nzchar(event)) stop("event must be a non-empty character scalar.", call. = FALSE)

  if (is.null(times)) times <- obs$times
  times <- sort(unique(as.numeric(times)))
  if (!all(times %in% obs$times)) stop("times must be a subset of obs$times.", call. = FALSE)

  start_time <- as.numeric(start_time)
  if (length(start_time) != 1L || !is.finite(start_time)) stop("start_time must be a finite numeric scalar.", call. = FALSE)
  if (!start_time %in% obs$times) stop("start_time must be one of obs$times.", call. = FALSE)
  if (!start_time %in% times) stop("start_time must be included in times.", call. = FALSE)

  if (is.null(obs$events) || is.null(obs$events$any) || is.null(obs$events$any[[event]])) {
    stop(sprintf("Event type '%s' not found in obs events.", event), call. = FALSE)
  }

  if (mode == "policy") {
    return(.compute_obs_risk_policy(obs, event = event, times = times, start_time = start_time, measured_only = measured_only))
  }
  .compute_obs_risk_interval(obs, event = event, times = times, start_time = start_time, measured_only = measured_only)
}

.compute_obs_risk_policy <- function(obs, event, times, start_time, measured_only) {
  t_idx <- match(times, obs$times)
  t0_idx <- match(start_time, obs$times)

  # Denominator fixed at t_ref (start_time)
  denom0 <- make_denom_mask(
    alive_mask = obs$alive_mask[, t0_idx, drop = FALSE],
    followup_defined = obs$followup_defined[, t0_idx, drop = FALSE],
    model_defined = NULL,
    measured = NULL,
    measured_only = FALSE
  )
  eligible <- as.logical(denom0[, 1])
  n_eligible <- sum(eligible)

  any_mat <- obs$events$any[[event]]  # [entities x (T-1)] integer

  # Compute first interval index of event for each entity
  first_int <- rep(NA_integer_, nrow(any_mat))
  for (i in seq_len(nrow(any_mat))) {
    idx <- which(any_mat[i, ] > 0L)
    if (length(idx) > 0L) first_int[i] <- min(idx)
  }

  # Optional at-risk gating: policy-mode can be gated at the interval of
  # occurrence (if at_risk provided) but does not re-weight denominators.
  at_risk_e <- NULL
  if (!is.null(obs$at_risk) && is.list(obs$at_risk) && !is.null(obs$at_risk[[event]])) {
    at_risk_e <- obs$at_risk[[event]]
  }

  n_events <- integer(length(t_idx))
  for (jj in seq_along(t_idx)) {
    k_time <- t_idx[jj]
    cutoff_int <- k_time - 1L
    if (cutoff_int < 1L) {
      n_events[jj] <- 0L
      next
    }
    occurred <- !is.na(first_int) & (first_int <= cutoff_int)
    if (!is.null(at_risk_e)) {
      which_occ <- which(occurred)
      if (length(which_occ) > 0L) {
        ok <- mapply(function(i, k) isTRUE(at_risk_e[i, k]), which_occ, first_int[which_occ])
        occurred[which_occ] <- ok
      }
    }
    n_events[jj] <- sum(occurred & eligible)
  }

  data.frame(
    time = times,
    n_eligible = rep.int(n_eligible, length(times)),
    n_events = n_events,
    risk = if (n_eligible == 0L) rep(NA_real_, length(times)) else n_events / n_eligible
  )
}

.compute_obs_risk_interval <- function(obs, event, times, start_time, measured_only) {
  t_idx <- match(times, obs$times)
  t0_idx <- match(start_time, obs$times)

  any_mat <- obs$events$any[[event]]  # [entities x (T-1)]
  at_risk_e <- NULL
  if (!is.null(obs$at_risk) && is.list(obs$at_risk) && !is.null(obs$at_risk[[event]])) {
    at_risk_e <- obs$at_risk[[event]]
  }

  # If at_risk not supplied, default to TRUE where alive&followup at interval start.
  n_events <- integer(length(t_idx))
  n_eligible <- integer(length(t_idx))

  for (jj in seq_along(t_idx)) {
    k_time <- t_idx[jj]
    if (k_time <= 1L) {
      n_events[jj] <- 0L
      n_eligible[jj] <- 0L
      next
    }
    # Interval indices contributing up to this time: 1..k_time-1
    cutoff_int <- k_time - 1L

    # Risk set at interval start for this cutoff interval (dynamic): use mask at time k_time-1
    denom_k <- make_denom_mask(
      alive_mask = obs$alive_mask[, k_time - 1L, drop = FALSE],
      followup_defined = obs$followup_defined[, k_time - 1L, drop = FALSE],
      model_defined = NULL,
      measured = NULL,
      measured_only = FALSE
    )
    elig_k <- as.logical(denom_k[, 1])

    if (!is.null(at_risk_e)) {
      elig_k <- elig_k & (at_risk_e[, cutoff_int] %in% TRUE)
    }

    n_eligible[jj] <- sum(elig_k)
    n_events[jj] <- sum((any_mat[, cutoff_int] > 0L) & elig_k)
  }

  data.frame(
    time = times,
    n_eligible = n_eligible,
    n_events = n_events,
    risk = ifelse(n_eligible == 0L, NA_real_, n_events / n_eligible)
  )
}
