
\name{chisq_ind}
\alias{chisq_ind}
\title{Chi-Square Test of Independence Using Simulation}

\description{
Performs a simulation-based chi-square test of independence
to determine whether two categorical variables are associated.
}

\usage{
chisq_ind(x, y = NULL, reps = 10000, plot = FALSE, data = NULL)
}

\arguments{
  \item{x}{A contingency table or the first categorical variable.}
  \item{y}{Optional second categorical variable. If NULL, x must be a contingency table.}
  \item{reps}{Number of simulation repetitions. Default is 10000.}
  \item{plot}{Logical. If TRUE, displays a histogram of the simulated chi-square statistics. Default is FALSE.}
  \item{data}{Optional data frame containing the variables.}
}

\value{
Simulation results, including the observed chi-square statistic,
simulation p-value, and number of extreme simulations.
}
