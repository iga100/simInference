
\name{randomization_two_mean_test}
\alias{randomization_two_mean_test}

\title{Randomization Test for Two Means}

\description{
Performs a simulation-based randomization test for the
difference between two population means.
}

\usage{
randomization_two_mean_test(
  x,
  group,
  group1 = NULL,
  group2 = NULL,
  alternative = "two.sided",
  reps = 10000,
  ci = FALSE,
  conf.level = 0.95,
  ci.method = c("percentile", "basic", "normal"),
  data = NULL
)
}

\arguments{
  \item{x}{Numeric observations to be compared.}

  \item{group}{Categorical variable identifying the two groups.}

  \item{group1}{Value identifying the first group.}

  \item{group2}{Value identifying the second group.}

  \item{alternative}{
    Alternative hypothesis: "two.sided", "less", or "greater".
  }

  \item{reps}{
    Number of randomization repetitions. Default is 10000.
  }

  \item{ci}{
    Logical value indicating whether to calculate a
    confidence interval.
  }

  \item{conf.level}{
    Confidence level. Default is 0.95.
  }

  \item{ci.method}{
    Confidence interval method: "percentile", "basic",
    or "normal".
  }

  \item{data}{
    Optional data frame containing the variables.
  }
}

\value{
Simulation results, including estimates, p-values,
and confidence intervals when requested.
}

