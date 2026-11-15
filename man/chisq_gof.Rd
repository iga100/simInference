
\name{chisq_gof}
\alias{chisq_gof}
\title{Chi-Square Goodness-of-Fit Test Using Simulation}

\description{
Performs a simulation-based chi-square goodness-of-fit test
for categorical data.
}

\usage{
chisq_gof(observed, p0 = NULL, reps = 10000, plot = FALSE, data = NULL)
}

\arguments{
  \item{observed}{Observed category counts.}
  \item{p0}{Expected category probabilities under the null hypothesis. If NULL, equal probabilities are assumed.}
  \item{reps}{Number of simulation repetitions. Default is 10000.}
  \item{plot}{Logical. If TRUE, displays a histogram of the simulated chi-square statistics. Default is FALSE.}
  \item{data}{Optional data frame containing the variable.}
}

\value{
Simulation results, including the observed chi-square statistic,
simulation p-value, and number of extreme simulations.
}
