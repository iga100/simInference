\name{chisq_gof}
\alias{chisq_gof}
\title{chisq gof}
\description{Teaching-oriented simulation inference function.}
\usage{
chisq_gof(observed, p0 = NULL, reps = 10000, plot = TRUE, data = NULL) 
NULL
}
\arguments{
  \item{observed}{Observed category counts.}
  \item{p0}{Proportion or category probabilities under the null hypothesis.}
  \item{reps}{Number of simulation repetitions.}
  \item{plot}{Whether to display a simulation plot.}
  \item{data}{Optional data frame containing the variables.}
}
\value{Simulation results, including estimates or confidence intervals as appropriate.}
