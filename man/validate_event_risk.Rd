\name{validate_event_risk}
\alias{validate_event_risk}
\title{Validate event risk curves against observed gridded data}
\description{
Compare predicted cumulative event risk curves (from \code{fluxForecast::event_prob()}) to observed
cumulative incidence computed from a \code{flux_obs_grid}.
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
\item{pred}{A \code{flux_forecast} or \code{flux_event_prob} (preferred) object (from \code{fluxForecast}).}

\item{obs}{A \code{flux_obs_grid} object (from \code{fluxValidation::build_obs_grid()}).}

\item{event}{Character scalar event type to validate.}

\item{start_time}{Optional scalar start time defining the eligible cohort. Must be one of \code{pred$times}. Defaults to \code{pred$start_time} for \code{flux_event_prob} or \code{pred$time0} for \code{flux_forecast}.}

\item{times}{Optional numeric subset of \code{pred$times} at which to evaluate risk. Defaults to all.}

\item{obs_mode}{Observed risk estimand. \code{"policy"} fixes the denominator at \code{start_time} (Forecast-compatible). \code{"interval"} uses interval-specific risk sets via \code{at_risk}.}
}
\value{
A list with components \code{predicted}, \code{observed}, and \code{meta}.
}
\details{
Observed risk curves are computed via \code{compute_obs_risk()} on the same \code{times} grid.
The default \code{obs_mode="policy"} matches the fixed-cohort estimand used by
\code{fluxForecast::event_prob()}.
}
