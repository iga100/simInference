
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
    An optional second categorical variable.
    When x is a contingency table, y is not required.
    Default is NULL.
  }

  \item{reps}{
    A positive integer specifying the number of simulations
    used to estimate the p-value. Default is 10000.
  }

  \item{plot}{
    Logical. If TRUE, displays a histogram of simulated
    chi-square statistics, highlighting the extreme outcomes.
    Default is FALSE.
  }

  \item{data}{
    An optional data frame containing the categorical
    variables used in the analysis. Default is NULL.
  }
}

\details{
  This function tests the null hypothesis that two categorical
  variables are independent against the alternative hypothesis
  that they are associated.

  The Pearson chi-square statistic is

  \deqn{\chi^2 = \sum \frac{(O-E)^2}{E}}

  where O and E denote observed and expected frequencies,
  respectively.

  The expected frequency for each cell is calculated as

  \deqn{E_{ij} = \frac{R_i C_j}{n}}

  where R_i and C_j represent the corresponding row and
  column totals, and n is the total number of observations.

  The function uses simulation to approximate the null
  distribution of the chi-square statistic.

  The simulation p-value is calculated as

  \deqn{p = \frac{b+1}{B+1}}

  where b is the number of simulated statistics at least
  as large as the observed statistic, and B is the
  number of simulations.

  Larger chi-square statistics provide stronger evidence
  against the null hypothesis of independence.

  The function does not require the user to specify a
  significance level. The resulting p-value can be compared
  with a chosen significance level, such as 0.05.
}

\value{
  Simulation-based test results, including the observed
  chi-square statistic, simulation p-value, and number
  of extreme simulations.

  When plot = TRUE, a histogram of the simulated
  chi-square statistics is displayed.
}

\examples{
# Example 1: Contingency table

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

# Example 3: Two categorical variables

gender <- c("Male", "Female", "Male", "Female",
            "Male", "Female", "Male", "Female")

preference <- c("Yes", "No", "Yes", "Yes",
                "No", "No", "Yes", "No")

chisq_ind(
  x = gender,
  y = preference,
  reps = 1000
)
}
