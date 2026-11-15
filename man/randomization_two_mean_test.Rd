
\name{randomization_two_mean_test}
\alias{randomization_two_mean_test}

\title{Simulation-Based Hypothesis Test for Two Population Means}

\description{
Performs a simulation-based randomization test for the
difference between two population means using permutation
of the observed data. Optionally calculates bootstrap
confidence intervals.
}

\usage{
randomization_two_mean_test(
  x,
  group,
  group1 = NULL,
  group2 = NULL,
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
  \item{x}{Numeric vector containing the observations to be compared.}

  \item{group}{Categorical variable identifying the two groups.}

  \item{group1}{Value identifying the first group. If NULL, the first available group is used.}

  \item{group2}{Value identifying the second group. If NULL, the second available group is used.}

  \item{alternative}{
    Alternative hypothesis: "two.sided", "less", or "greater".
    Default is "two.sided".
  }

  \item{reps}{
    Number of randomization repetitions. Default is 10000.
  }

  \item{plot}{
    Logical. If TRUE, displays a histogram of the simulated
    differences in sample means. Default is FALSE.
  }

  \item{ci}{
    Logical. If TRUE, calculates bootstrap confidence intervals.
    Default is FALSE.
  }

  \item{conf.level}{
    Confidence level for the bootstrap intervals.
    Default is 0.95.
  }

  \item{ci.method}{
    Bootstrap confidence interval methods: "percentile",
    "basic", or "normal".
  }

  \item{data}{
    Optional data frame containing the variables.
  }
}

\value{
An invisible list containing the group sample sizes, sample means,
standard deviations, observed difference in means, simulation
p-value, number of extreme simulations, simulated differences,
and optional bootstrap confidence intervals.
}

\examples{
x <- c(12, 14, 15, 16, 18, 20, 22, 24)
group <- c("A", "A", "A", "A", "B", "B", "B", "B")

set.seed(123)

# Run the hypothesis test without a histogram
randomization_two_mean_test(
  x, group, reps = 1000
)

# Display the simulation histogram
randomization_two_mean_test(
  x, group, reps = 1000, plot = TRUE
)
}
