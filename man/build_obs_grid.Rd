
\name{build_obs_grid}
\alias{build_obs_grid}
\title{Align observed patient data to a forecast grid}
\description{
Align observed/test-set patient data to a forecast evaluation grid.
}
\usage{
build_obs_grid(
  vars,
  events = NULL,
  times,
  t0,
  start_time = 0,
  time_unit = NULL,
  id_col = "patient_id",
  time_col = "time",
  event_time_col = "event_time",
  event_type_col = "event_type",
  measure_lookback = 0,
  model_defined = NULL,
  at_risk = NULL
)
}
\arguments{
\item{vars}{A non-empty list of data.frames. Each data.frame must contain \code{patient_id} and optionally \code{time}. If \code{time} is absent, all non-id columns are treated as always-defined variables. If \code{time} is present, all non-id/time columns are treated as longitudinal measurements. Duplicate variable names across tables error in v0.0.2.}

\item{events}{Optional data.frame with \code{patient_id}, \code{event_time}, and \code{event_type}.}

\item{times}{Numeric vector of non-decreasing offsets defining the evaluation grid (relative to patient-specific t0).}

\item{t0}{A scalar (applied to all patients) or a function returning a per-patient anchor time (numeric, Date, or POSIXct).}

\item{start_time}{Numeric offset applied to \code{times} before adding to \code{t0}.}

\item{time_unit}{Required when \code{t0} is Date/POSIXct. Supported: days, hours, minutes, seconds.}

\item{id_col}{Patient id column name.}
\item{time_col}{Time column name for vars tables.}
\item{event_time_col}{Time column name for events table.}
\item{event_type_col}{Event type column name for events table.}
\item{measure_lookback}{Non-negative lookback window for measuredness. Default 0 means exact match.}
}
\value{A \code{ps_obs_grid} object.}
\details{
This is v0.0.2: contracts first, metrics later.

If \code{death_date} and/or \code{last_contact_date} are provided as always-defined variables, they are used to derive \code{alive_mask} (TRUE/FALSE/NA) and \code{followup_defined} (TRUE/FALSE) on the grid.
}
\item{model_defined}{Optional named list of logical matrices \code{[n_patients x n_times]} keyed by variable name, indicating when a variable is in-scope/meaningful on the grid. Defaults to all TRUE.}

\item{at_risk}{Optional named list of logical matrices \code{[n_patients x (n_times-1)]} keyed by event type, indicating when patients are at-risk for event occurrence in each interval. Defaults to all TRUE for observed events.}

