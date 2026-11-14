\name{bootstrap_one_prop_ci}
\alias{bootstrap_one_prop_ci}
\title{bootstrap one prop ci}
\description{Teaching-oriented simulation inference function.}
\usage{
bootstrap_one_prop_ci(x, success = "Yes", reps = 10000, conf.level = 0.95, 
    ci.method = c("percentile", "basic", "normal"), data = NULL) 
NULL
}
\arguments{
  \item{x}{Input variable or observations.}
  \item{success}{Value identifying a successful outcome.}
  \item{reps}{Number of simulation repetitions.}
  \item{conf.level}{Confidence level.}
  \item{ci.method}{Method used to construct the confidence interval.}
  \item{data}{Optional data frame containing the variables.}
}
\value{Simulation results, including estimates or confidence intervals as appropriate.}
