\name{validate_event_risk}
\alias{validate_event_risk}
\title{Validate event risk curves against observed gridded data}
\description{
Compare predicted cumulative event risk curves (from \code{patientSimForecast::risk()}) to observed
cumulative incidence computed from a \code{ps_obs_grid}.
}
\usage{
validate_event_risk(
  pred,
  obs,
  event,
  start_time = NULL,
  times = NULL,
  obs_mode = c("policy", "interval")
)
}
\arguments{
\item{pred}{A \code{ps_forecast} or \code{ps_risk} (preferred) object (from \code{patientSimForecast}).}

\item{obs}{A \code{ps_obs_grid} object (from \code{patientSimValidation::build_obs_grid()}).}

\item{event}{Character scalar event type to validate.}

\item{start_time}{Optional scalar start time defining the eligible cohort. Must be one of \code{pred$times}. Defaults to \code{pred$start_time} for \code{ps_risk} or \code{pred$time0} for \code{ps_forecast}.}

\item{times}{Optional numeric subset of \code{pred$times} at which to evaluate risk. Defaults to all.}

\item{obs_mode}{Observed risk estimand. \code{"policy"} fixes the denominator at \code{start_time} (Forecast-compatible). \code{"interval"} uses interval-specific risk sets via \code{at_risk}.}
}
\value{
A list with components \code{predicted}, \code{observed}, and \code{meta}.
}
\details{
Observed risk curves are computed via \code{compute_obs_risk()} on the same \code{times} grid.
The default \code{obs_mode="policy"} matches the fixed-cohort estimand used by
\code{patientSimForecast::risk()}.
}
