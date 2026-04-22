
\name{build_obs_grid}
\alias{build_obs_grid}
\title{Align observed entity data to a forecast grid}
\description{
Align observed/test-set entity data to a forecast evaluation grid.
}
\usage{
build_obs_grid(
  vars,
  events = NULL,
  times,
  t0,
  start_time = 0,
  ctx = NULL,
  time_spec = NULL,
  schema = NULL,
  default_window = 0,
  window = NULL,
  id_col = "entity_id",
  time_col = "time",
  event_time_col = "event_time",
  event_type_col = "event_type",
  measure_lookback = 0,
  model_defined = NULL,
  at_risk = NULL
)
}
\arguments{
\item{vars}{A non-empty list of data.frames. Each data.frame must contain \code{entity_id} and optionally \code{time}. If \code{time} is absent, all non-id columns are treated as always-defined variables. If \code{time} is present, all non-id/time columns are treated as longitudinal measurements. Duplicate variable names across tables error in v0.0.2.}

\item{events}{Optional data.frame with \code{entity_id}, \code{event_time}, and \code{event_type}.}

\item{times}{Numeric vector of non-decreasing offsets defining the evaluation grid (relative to entity-specific t0).}

\item{t0}{A scalar (applied to all entities) or a function returning a per-entity anchor time (numeric, Date, or POSIXct).}

\item{start_time}{Numeric offset applied to \code{times} before adding to \code{t0}.}

\item{ctx}{Optional context list. Compatibility path for time metadata via \code{ctx$time} or \code{ctx$time_spec}.}

\item{time_spec}{Optional compiled \code{fluxCore::time_spec()} object used for calendar-to-model-time conversion.}

\item{schema}{Optional fluxCore schema used to expand grouped window settings.}

\item{default_window}{Default LOCF window for gridding observed state.}

\item{window}{Optional named per-variable (or group) window overrides.}

\item{id_col}{Entity id column name.}
\item{time_col}{Time column name for vars tables.}
\item{event_time_col}{Time column name for events table.}
\item{event_type_col}{Event type column name for events table.}
\item{measure_lookback}{Non-negative lookback window for measuredness. Default 0 means exact match.}
\item{model_defined}{Optional named list of logical matrices indicating model-defined cells by variable.}
\item{at_risk}{Optional named list of logical interval masks for event risk sets.}
}
\value{A \code{flux_obs_grid} object.}
\details{
This is v0.0.2: contracts first, metrics later.

If \code{death_date} and/or \code{last_contact_date} are provided as always-defined variables, they are used to derive \code{alive_mask} (TRUE/FALSE/NA) and \code{followup_defined} (TRUE/FALSE) on the grid.
}
