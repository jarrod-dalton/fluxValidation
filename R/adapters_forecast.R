# ------------------------------------------------------------------------------
# Adapters: convert flux_forecast (fluxForecast) objects into lightweight
# summary objects preferred by fluxValidation.
#
# Validation remains read-only with respect to Forecast outputs.
# ------------------------------------------------------------------------------

.require_forecast <- function() {
  if (!requireNamespace("fluxForecast", quietly = TRUE)) {
    stop(
      "fluxForecast is required for this operation but is not installed.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Coerce prediction input to an event-probability object
#'
#' Converts forecast inputs to an event-probability summary object for validation workflows.
#'
#' @param x Either a `flux_event_prob` (returned unchanged) or a `flux_forecast`.
#' @param ... Passed to [fluxForecast::event_prob()] when `x` is a `flux_forecast`.
#'
#' @return A `flux_event_prob` object.
#' @export
as_event_prob <- function(x, ...) {
  if (inherits(x, "flux_event_prob")) return(x)
  if (inherits(x, "flux_forecast")) {
    .require_forecast()
    f <- getExportedValue("fluxForecast", "event_prob")
    return(f(x, ...))
  }
  stop("x must be a flux_event_prob or flux_forecast.", call. = FALSE)
}

#' Coerce prediction input to a state-probability summary
#'
#' Converts forecast inputs to categorical/binary/ordinal state probability summaries for validation.
#'
#' @param x Either a `flux_state_prob` (returned unchanged) or a `flux_forecast`.
#' @param var Variable name.
#' @param times Optional subset of times.
#' @param start_time Optional start time used to align estimands.
#' @param by Grouping passed to [fluxForecast::state_summary()].
#' @param categorical_max_levels Maximum levels forwarded to [fluxForecast::state_summary()].
#'
#' @return A `flux_state_prob` object.
#' @export
as_state_prob <- function(
  x,
  var,
  times = NULL,
  start_time = NULL,
  by = c("run", "entity", "entity_param_draw"),
  categorical_max_levels = 50
) {
  if (inherits(x, "flux_state_prob")) return(x)
  if (!inherits(x, "flux_forecast")) stop("x must be a flux_state_prob or flux_forecast.", call. = FALSE)

  .require_forecast()

  by <- match.arg(by)

  # state_summary returns a named list of data.frames (one per var)
  ss <- fluxForecast::state_summary(
    x,
    vars = var,
    times = times,
    categorical_max_levels = categorical_max_levels,
    by = by
  )
  df <- ss[[var]]
  if (is.null(df) || nrow(df) == 0) {
    # Preserve declared levels if available
    lev <- NULL
    if (!is.null(x$meta) && !is.null(x$meta$schema) && !is.null(x$meta$schema[[var]]) && !is.null(x$meta$schema[[var]]$levels)) {
      lev <- as.character(x$meta$schema[[var]]$levels)
    }
    if (is.null(lev)) lev <- character(0)
    spec <- list(var = var, times = if (is.null(times)) x$times else as.numeric(times), start_time = if (is.null(start_time)) x$time0 else as.numeric(start_time), levels = lev)
    return(new_state_prob(spec = spec, result = data.frame(time = numeric(0), level = character(0), prob = numeric(0))))
  }

  lev <- unique(as.character(df$level))
  # Prefer schema-declared order if present
  if (!is.null(x$meta) && !is.null(x$meta$schema) && !is.null(x$meta$schema[[var]]) && !is.null(x$meta$schema[[var]]$levels)) {
    lev <- as.character(x$meta$schema[[var]]$levels)
  }

  if (is.null(times)) times <- sort(unique(as.numeric(df$time)))
  if (is.null(start_time)) start_time <- x$time0

  res <- data.frame(
    time = as.numeric(df$time),
    level = as.character(df$level),
    prob = as.numeric(df$p),
    stringsAsFactors = FALSE
  )

  spec <- list(var = var, times = as.numeric(times), start_time = as.numeric(start_time), levels = lev)
  new_state_prob(spec = spec, result = res)
}

#' Coerce prediction input to a state-point summary
#'
#' Converts forecast inputs to continuous-ish state point summaries for validation.
#'
#' @param x Either a `flux_state_point` (returned unchanged) or a `flux_forecast`.
#' @param var Variable name.
#' @param times Optional subset of times.
#' @param start_time Optional start time used to align estimands.
#' @param by Grouping passed to [fluxForecast::state_summary()].
#'
#' @return A `flux_state_point` object.
#' @export
as_state_point <- function(
  x,
  var,
  times = NULL,
  start_time = NULL,
  by = c("run", "entity", "entity_param_draw")
) {
  if (inherits(x, "flux_state_point")) return(x)
  if (!inherits(x, "flux_forecast")) stop("x must be a flux_state_point or flux_forecast.", call. = FALSE)

  .require_forecast()

  by <- match.arg(by)

  ss <- fluxForecast::state_summary(x, vars = var, times = times, by = by)
  df <- ss[[var]]

  if (is.null(df) || nrow(df) == 0) {
    spec <- list(var = var, times = if (is.null(times)) x$times else as.numeric(times), start_time = if (is.null(start_time)) x$time0 else as.numeric(start_time))
    return(new_state_point(spec = spec, result = data.frame(time = numeric(0), mean = numeric(0))))
  }

  if (is.null(times)) times <- sort(unique(as.numeric(df$time)))
  if (is.null(start_time)) start_time <- x$time0

  res <- df
  # Standardize column naming for validation
  if (!("mean" %in% names(res)) && ("mean" %in% tolower(names(res)))) {
    # unlikely, but defensive
    names(res)[tolower(names(res)) == "mean"] <- "mean"
  }
  res$time <- as.numeric(res$time)

  spec <- list(var = var, times = as.numeric(times), start_time = as.numeric(start_time))
  new_state_point(spec = spec, result = res[, intersect(names(res), c("time", "n", "mean", "sd", "min", "q1", "median", "q3", "max")), drop = FALSE])
}
