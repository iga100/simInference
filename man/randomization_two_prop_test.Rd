
\name{randomization_two_prop_test}
\alias{randomization_two_prop_test}

\title{Simulation-Based Hypothesis Test for Two Population Proportions}

\description{
Performs a simulation-based randomization test for the
difference between two population proportions using permutation
of the observed group labels. Optionally calculates bootstrap
confidence intervals.
}

\usage{
randomization_two_prop_test(
  x,
  group,
  success = 1,
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
  \item{x}{
    Vector containing the observed categorical responses.
  }

  \item{group}{
    Categorical variable identifying the two groups.
  }

  \item{success}{
    Value identifying a successful outcome. Default is 1.
  }

  \item{group1}{
    Value identifying the first group to compare.
    If NULL, the function selects a group automatically.
  }

  \item{group2}{
    Value identifying the second group to compare.
    If NULL, the function selects a group automatically.
  }

  \item{alternative}{
    Alternative hypothesis: "two.sided", "less", or "greater".
    Default is "two.sided".
  }

  \item{reps}{
    Number of randomization repetitions. Default is 10000.
  }

  \item{plot}{
    Logical. If TRUE, displays a histogram of the simulated
    differences in sample proportions. Default is FALSE.
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
Simulation results, including the observed sample proportions,
difference in sample proportions, simulation p-value, and
optional bootstrap confidence intervals.
}

\examples{
x <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1)
group <- c("A", "A", "A", "A", "A",
           "B", "B", "B", "B", "B")

set.seed(123)

# Run the hypothesis test without a histogram
randomization_two_prop_test(
  x, group, success = 1, reps = 1000
)

# Display the simulation histogram
randomization_two_prop_test(
  x, group, success = 1,
  reps = 1000, plot = TRUE
)
}
