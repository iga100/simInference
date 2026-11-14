\name{randomization_one_prop_test}
\alias{randomization_one_prop_test}
\title{randomization one prop test}
\description{Teaching-oriented simulation inference function.}
\usage{
randomization_one_prop_test(x, success = 1, p0, alternative = "two.sided", reps = 10000, 
    ci = FALSE, conf.level = 0.95, ci.method = c("percentile", 
        "basic", "normal"), data = NULL) 
NULL
}
\arguments{
  \item{x}{Input variable or observations.}
  \item{success}{Value identifying a successful outcome.}
  \item{p0}{Proportion or category probabilities under the null hypothesis.}
  \item{alternative}{Alternative hypothesis.}
  \item{reps}{Number of simulation repetitions.}
  \item{ci}{Whether to calculate a confidence interval.}
  \item{conf.level}{Confidence level.}
  \item{ci.method}{Method used to construct the confidence interval.}
  \item{data}{Optional data frame containing the variables.}
}
\value{Simulation results, including estimates or confidence intervals as appropriate.}
