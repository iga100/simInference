\name{randomization_two_prop_test}
\alias{randomization_two_prop_test}
\title{randomization two prop test}
\description{Teaching-oriented simulation inference function.}
\usage{
randomization_two_prop_test(x, group, success = 1, group1 = NULL, group2 = NULL, 
    alternative = "two.sided", reps = 10000, ci = FALSE, conf.level = 0.95, 
    ci.method = c("percentile", "basic", "normal"), data = NULL) 
NULL
}
\arguments{
  \item{x}{Input variable or observations.}
  \item{group}{Grouping variable.}
  \item{success}{Value identifying a successful outcome.}
  \item{group1}{First group to compare.}
  \item{group2}{Second group to compare.}
  \item{alternative}{Alternative hypothesis.}
  \item{reps}{Number of simulation repetitions.}
  \item{ci}{Whether to calculate a confidence interval.}
  \item{conf.level}{Confidence level.}
  \item{ci.method}{Method used to construct the confidence interval.}
  \item{data}{Optional data frame containing the variables.}
}
\value{Simulation results, including estimates or confidence intervals as appropriate.}
