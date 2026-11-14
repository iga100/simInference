\name{bootstrap_one_mean_ci}
\alias{bootstrap_one_mean_ci}
\title{bootstrap one mean ci}
\description{Teaching-oriented simulation inference function.}
\usage{
bootstrap_one_mean_ci(x, reps = 10000, conf.level = 0.95, ci.method = c("percentile", 
    "basic", "normal"), data = NULL) 
NULL
}
\arguments{
  \item{x}{Input variable or observations.}
  \item{reps}{Number of simulation repetitions.}
  \item{conf.level}{Confidence level.}
  \item{ci.method}{Method used to construct the confidence interval.}
  \item{data}{Optional data frame containing the variables.}
}
\value{Simulation results, including estimates or confidence intervals as appropriate.}
