
\name{chisq_ind}
\alias{chisq_ind}

\title{Simulation-Based Chi-Square Test of Independence}

\description{
Performs a simulation-based chi-square test of independence
to investigate whether two categorical variables are associated.
The p-value is estimated from a simulated null distribution
rather than an asymptotic chi-square distribution.
}

\usage{
chisq_ind(
  x,
  y = NULL,
  reps = 10000,
  plot = FALSE,
  data = NULL
)
}

\arguments{
  \item{x}{
    A contingency table of observed frequencies or a
    categorical variable representing the first variable
    in the analysis.
  }

  \item{y}{
    An optional second categorical variable of the same
    length as x. When x is a contingency table, y is
    not required. Default is NULL.
  }

  \item{reps}{
    A positive integer specifying the number of simulations
    used to estimate the p-value. Default is 10000.
  }

  \item{plot}{
    Logical. If TRUE, displays a histogram of simulated
    chi-square statistics, highlighting values at least
    as large as the observed statistic.
    Default is FALSE.
  }

  \item{data}{
    An optional data frame containing the categorical
    variables used in the analysis. Default is NULL.
  }
}

\details{
  This function tests whether two categorical variables
  are independent in the population.

  The hypotheses are:

  \itemize{
    \item H0: The two categorical variables are independent.
    \item Ha: The two categorical variables are associated.
  }

  The Pearson chi-square statistic is calculated as

  \deqn{\chi^2 = \sum \frac{(O-E)^2}{E}}

  where O represents the observed frequency and E
  represents the expected frequency under the null
  hypothesis of independence.

  The expected frequency for each cell is calculated as

  \deqn{E_{ij} = \frac{R_i C_j}{n}}

  where R_i represents the corresponding row total,
  C_j represents the corresponding column total,
  and n is the total number of observations.

  The function uses simulation to approximate the null
  distribution of the chi-square statistic.

  The simulation p-value is calculated as

  \deqn{p = \frac{b+1}{B+1}}

  where b is the number of simulated chi-square statistics
  at least as large as the observed statistic, and B is
  the total number of simulations.

  Larger chi-square statistics provide stronger evidence
  against the null hypothesis of independence.

  The function does not require the user to specify a
  significance level. The resulting p-value can be
  compared with a chosen significance level, such as 0.05.

  Unlike the conventional asymptotic chi-square test,
  this function estimates the p-value using simulation.

  When plot = TRUE, a histogram of the simulated
  chi-square statistics is displayed, with extreme
  outcomes highlighted.
}

\value{
  An object of class \code{"chisq_sim_result"} containing
  the following components:

  \describe{
    \item{test}{
      Character string identifying the statistical test.
    }

    \item{statistic}{
      Observed Pearson chi-square test statistic.
    }

    \item{p_value}{
      Simulation-based p-value for the test.
    }

    \item{extreme_count}{
      Number of simulated chi-square statistics at least
      as large as the observed statistic.
    }

    \item{reps}{
      Number of simulations performed.
    }

    \item{observed}{
      Contingency table of observed frequencies.
    }

    \item{expected}{
      Contingency table of expected frequencies under
      the null hypothesis of independence.
    }

    \item{simulated_statistics}{
      Numeric vector containing the simulated chi-square
      statistics used to estimate the p-value.
    }
  }
}

\examples{
# Example 1: Using a contingency table

observed <- matrix(
  c(20, 30, 25, 25),
  nrow = 2
)

set.seed(123)

chisq_ind(
  x = observed,
  reps = 1000
)

# Example 2: Display the simulation histogram

chisq_ind(
  x = observed,
  reps = 1000,
  plot = TRUE
)

# Example 3: Using two categorical variables

gender <- c(
  "Male", "Female", "Male", "Female",
  "Male", "Female", "Male", "Female"
)

preference <- c(
  "Yes", "No", "Yes", "Yes",
  "No", "No", "Yes", "No"
)

chisq_ind(
  x = gender,
  y = preference,
  reps = 1000
)

# Example 4: Access individual results

result <- chisq_ind(
  x = observed,
  reps = 1000
)

result$statistic
result$p_value
result$observed
result$expected
result$simulated_statistics
}
