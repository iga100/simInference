\name{bootstrap_two_mean_ci}
\alias{bootstrap_two_mean_ci}
\title{bootstrap two mean ci}
\description{Teaching-oriented simulation inference function.}
\usage{
bootstrap_two_mean_ci(x, group, group1 = NULL, group2 = NULL, reps = 10000, 
    conf.level = 0.95, ci.method = c("percentile", "basic", "normal"), 
    data = NULL) 
NULL
}
\arguments{
  \item{x}{Input variable or observations.}
  \item{group}{Grouping variable.}
  \item{group1}{First group to compare.}
  \item{group2}{Second group to compare.}
  \item{reps}{Number of simulation repetitions.}
  \item{conf.level}{Confidence level.}
  \item{ci.method}{Method used to construct the confidence interval.}
  \item{data}{Optional data frame containing the variables.}
}
\value{Simulation results, including estimates or confidence intervals as appropriate.}
