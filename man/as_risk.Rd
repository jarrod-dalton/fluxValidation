\name{as_risk}
\alias{as_risk}
\title{Coerce prediction input to a risk object}
\usage{as_risk(x, ...)}
\arguments{
\item{x}{Either a \code{ps_risk} (returned unchanged) or a \code{ps_forecast}.}
\item{...}{Passed to \code{patientSimForecast::risk()} when \code{x} is a \code{ps_forecast}.}
}
\value{A \code{ps_risk} object.}
\description{Converts forecast inputs to a risk summary object for validation workflows.}
