# ------------------------------------------------------------------------------
# Lightweight prediction summary classes for validation
#
# These are intentionally minimal so patientSimValidation can validate against
# summary objects (preferred) without requiring full ps_forecast objects.
# ------------------------------------------------------------------------------

#' Construct a categorical/binary state probability summary
#'
#' @param spec List with fields: var (character scalar), times (numeric),
#'   start_time (numeric scalar), levels (character vector).
#' @param result data.frame with columns: time, level, prob.
#'
#' @keywords internal
new_ps_state_prob <- function(spec, result) {
  x <- list(spec = spec, result = result)
  class(x) <- "ps_state_prob"
  validate_ps_state_prob(x)
  x
}

#' @keywords internal
validate_ps_state_prob <- function(x) {
  if (!is.list(x) || is.null(x$spec) || is.null(x$result)) {
    stop("ps_state_prob must be a list with spec and result.", call. = FALSE)
  }
  s <- x$spec
  if (!is.list(s) || is.null(s$var) || is.null(s$times) || is.null(s$start_time)) {
    stop("ps_state_prob$spec must include var, times, start_time.", call. = FALSE)
  }
  if (!is.data.frame(x$result) || !all(c("time", "level", "prob") %in% names(x$result))) {
    stop("ps_state_prob$result must be a data.frame with columns time, level, prob.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Construct a point/quantile state summary (continuous-ish)
#'
#' @param spec List with fields: var (character scalar), times (numeric),
#'   start_time (numeric scalar).
#' @param result data.frame with at minimum columns: time, mean.
#'
#' @keywords internal
new_ps_state_point <- function(spec, result) {
  x <- list(spec = spec, result = result)
  class(x) <- "ps_state_point"
  validate_ps_state_point(x)
  x
}

#' @keywords internal
validate_ps_state_point <- function(x) {
  if (!is.list(x) || is.null(x$spec) || is.null(x$result)) {
    stop("ps_state_point must be a list with spec and result.", call. = FALSE)
  }
  s <- x$spec
  if (!is.list(s) || is.null(s$var) || is.null(s$times) || is.null(s$start_time)) {
    stop("ps_state_point$spec must include var, times, start_time.", call. = FALSE)
  }
  if (!is.data.frame(x$result) || !all(c("time", "mean") %in% names(x$result))) {
    stop("ps_state_point$result must be a data.frame with columns time, mean.", call. = FALSE)
  }
  invisible(TRUE)
}
