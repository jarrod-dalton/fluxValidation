\name{as_state_prob}
\alias{as_state_prob}
\title{Coerce prediction input to a state-probability summary}
\usage{
as_state_prob(
  x,
  var,
  times = NULL,
  start_time = NULL,
  by = c("run", "entity", "entity_param_draw"),
  categorical_max_levels = 50
)
}
\arguments{
\item{x}{Either a \code{flux_state_prob} (returned unchanged) or a \code{flux_forecast}.}
\item{var}{Variable name.}
\item{times}{Optional subset of times.}
\item{start_time}{Optional start time used to align estimands.}
\item{by}{Grouping passed to \code{fluxForecast::state_summary()}.}
\item{categorical_max_levels}{Maximum levels forwarded to \code{state_summary()}.}
}
\value{A \code{flux_state_prob} object.}
\description{Converts forecast inputs to categorical/binary/ordinal state probability summaries for validation.}
