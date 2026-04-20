\name{as_event_prob}
\alias{as_event_prob}
\title{Coerce prediction input to an event-probability object}
\usage{as_event_prob(x, ...)}
\arguments{
\item{x}{Either a \code{flux_event_prob} (returned unchanged) or a \code{flux_forecast}.}
\item{...}{Passed to \code{fluxForecast::event_prob()} when \code{x} is a \code{flux_forecast}.}
}
\value{A \code{flux_event_prob} object.}
\description{Converts forecast inputs to an event-probability summary object for validation workflows.}
