\name{validate_state_prob}
\alias{validate_state_prob}
\title{Validate categorical/binary state variables against predicted level probabilities}
\description{\code{validate_state_prob} compares observed, grid-aligned state values from a \code{ps_obs_grid} to predicted level probabilities supplied as a \code{ps_state_prob} summary object (or a compatible list). Denominators are computed from explicit masks, not from missing values.}
\usage{
validate_state_prob(pred, obs, var, times = NULL, start_time = NULL, measured_only = FALSE)
}
\arguments{
  \item{pred}{A \code{ps_state_prob} summary (preferred) or a list with \code{$spec} and \code{$result}.}
  \item{obs}{A \code{ps_obs_grid} produced by \code{build_obs_grid()}. Must include \code{obs$state[[var]]}.}
  \item{var}{Variable name to validate (schema naming convention).}
  \item{times}{Optional numeric vector of times to validate. Defaults to \code{pred$spec$times}.}
  \item{start_time}{Optional scalar reference time. Defaults to \code{pred$spec$start_time}.}
  \item{measured_only}{If \code{TRUE}, restrict denominators to measured cells using \code{obs$measured[[var]]}.}
}
\value{A list with \code{predicted}, \code{observed}, and \code{meta}.}
