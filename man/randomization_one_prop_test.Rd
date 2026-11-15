
\name{randomization_one_prop_test}
\alias{randomization_one_prop_test}
\title{Simulation-Based Hypothesis Test for One Population Proportion}

\description{
Performs a simulation-based hypothesis test for one population
proportion using samples generated under the null hypothesis.
Optionally calculates bootstrap confidence intervals.
}

\usage{
randomization_one_prop_test(
  x,
  success = 1,
  p0,
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
  \item{x}{Vector containing the observed categorical responses.}
  \item{success}{Value identifying a successful outcome. Default is 1.}
  \item{p0}{Hypothesized population proportion under the null hypothesis. Must be between 0 and 1.}
  \item{alternative}{Alternative hypothesis: "less", "greater", or "two.sided". Default is "two.sided".}
  \item{reps}{Number of simulation repetitions. Default is 10000.}
  \item{plot}{Logical. If TRUE, displays a histogram of the simulated sample proportions. Default is FALSE.}
  \item{ci}{Logical. If TRUE, calculates bootstrap confidence intervals. Default is FALSE.}
  \item{conf.level}{Confidence level for the bootstrap intervals. Default is 0.95.}
  \item{ci.method}{Bootstrap confidence interval methods: "percentile", "basic", or "normal".}
  \item{data}{Optional data frame containing the response variable.}
}

\value{
An invisible list containing the sample size, number of successes,
observed sample proportion, hypothesized proportion, simulation
p-value, number of extreme simulations, simulated sample proportions,
and optional bootstrap confidence intervals.
}

\examples{
x <- c(1, 1, 0, 1, 1, 0, 1, 1, 0, 1)

set.seed(123)

# Run the hypothesis test without a histogram
randomization_one_prop_test(
  x, success = 1, p0 = 0.5, reps = 1000
)

# Display the simulation histogram
randomization_one_prop_test(
  x, success = 1, p0 = 0.5,
  reps = 1000, plot = TRUE
)
}
