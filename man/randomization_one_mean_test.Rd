
\name{randomization_one_mean_test}
\alias{randomization_one_mean_test}
\title{Simulation-Based Hypothesis Test for One Population Mean}

\description{
Performs a simulation-based hypothesis test for one population mean
using resampling under the null hypothesis. Optionally calculates
bootstrap confidence intervals.
}

\usage{
randomization_one_mean_test(
  x,
  mu0,
  alternative = "two.sided",
  reps = 10000,
  plot = FALSE,
  ci = FALSE,
  conf.level = 0.95,
  ci.method = c("percentile", "basic", "normal"),
  data = NULL
)
}

\arguments{
  \item{x}{Numeric vector containing the sample observations.}
  \item{mu0}{Hypothesized population mean under the null hypothesis.}
  \item{alternative}{Alternative hypothesis: "less", "greater", or "two.sided". Default is "two.sided".}
  \item{reps}{Number of simulation repetitions. Default is 10000.}
  \item{plot}{Logical. If TRUE, displays a histogram of the simulated sample means. Default is FALSE.}
  \item{ci}{Logical. If TRUE, calculates bootstrap confidence intervals. Default is FALSE.}
  \item{conf.level}{Confidence level for the bootstrap intervals. Default is 0.95.}
  \item{ci.method}{Bootstrap confidence interval methods: "percentile", "basic", or "normal".}
  \item{data}{Optional data frame containing the variable.}
}

\value{
An invisible list containing the sample mean, hypothesized mean,
simulation p-value, number of extreme simulations, simulated
sample means, and optional bootstrap confidence intervals.
}

\examples{
x <- c(12, 14, 15, 16, 18, 20, 22, 24)

set.seed(123)

# Run the hypothesis test without a histogram
randomization_one_mean_test(x, mu0 = 15, reps = 1000)

# Display the simulation histogram
randomization_one_mean_test(
  x, mu0 = 15, reps = 1000, plot = TRUE
)
}

