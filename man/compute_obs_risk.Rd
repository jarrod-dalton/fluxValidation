\name{compute_obs_risk}
\alias{compute_obs_risk}
\title{Compute observed event risk curves on a forecast grid}
\description{
Compute observed event risk curves from a \code{ps_obs_grid}. This is a low-level helper
used by \code{validate_event_risk()}.
}
\usage{
compute_obs_risk(obs, event, times = NULL, start_time, mode = c("policy", "interval"), measured_only = FALSE)
}
\arguments{
\item{obs}{A \code{ps_obs_grid}.}
\item{event}{Character scalar event type.}
\item{times}{Optional numeric subset of \code{obs$times}. Defaults to all.}
\item{start_time}{Scalar reference time in \code{obs$times} at which to define the eligible cohort.}
\item{mode}{\code{"policy"} fixes the denominator at \code{start_time} (Forecast-compatible). \code{"interval"} uses interval-specific risk sets via \code{obs$at_risk}.}
\item{measured_only}{Reserved for future use; currently ignored for events.}
}
\value{A data.frame with columns \code{time}, \code{n_eligible}, \code{n_events}, and \code{risk}.}