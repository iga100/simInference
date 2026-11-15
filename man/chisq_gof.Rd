
\name{chisq_gof}
\alias{chisq_gof}

\title{Simulation-Based Chi-Square Goodness-of-Fit Test}

\description{
Performs a simulation-based chi-square goodness-of-fit test
to determine whether the observed frequencies of a categorical
variable are consistent with specified population probabilities.
The p-value is estimated using simulation rather than the
asymptotic chi-square distribution.
}

\usage{
chisq_gof(
  observed,
  p0 = NULL,
  reps = 10000,
  plot = FALSE,
  data = NULL
)
}

\arguments{
  \item{observed}{
    A numeric vector containing the observed frequencies
    for each category.
  }

  \item{p0}{
    A numeric vector specifying the hypothesized population
    probabilities for the categories. The probabilities
    must be nonnegative and sum to 1.
    If NULL, equal probabilities are assumed.
    Default is NULL.
  }

  \item{reps}{
    A positive integer specifying the number of simulations
    used to estimate the p-value. Default is 10000.
  }

  \item{plot}{
    Logical. If TRUE, displays a histogram of the simulated
    chi-square statistics, highlighting values at least
    as large as the observed statistic.
    Default is FALSE.
  }

  \item{data}{
    An optional data frame containing the categorical
    variable used in the analysis. Default is NULL.
  }
}

\details{
  The chi-square goodness-of-fit test examines whether
  the distribution of a categorical variable agrees
  with a hypothesized population distribution.

  The hypotheses are:

  \itemize{
    \item H0: The population category probabilities equal
    the hypothesized probabilities.
    \item Ha: At least one population category probability
    differs from its hypothesized value.
  }

  The Pearson chi-square statistic is calculated as

  \deqn{\chi^2 = \sum_{i=1}^{k} \frac{(O_i-E_i)^2}{E_i}}

  where O_i represents the observed frequency,
  E_i represents the expected frequency, and
  k is the number of categories.

  Expected frequencies are calculated as

  \deqn{E_i = n p_{0i}}

  where n is the total sample size and p_{0i}
  is the hypothesized probability for category i.

  If p0 is NULL, the hypothesized probabilities
  are equal across all categories:

  \deqn{p_{0i} = \frac{1}{k}}

  The function generates simulated samples under
  the null hypothesis using the hypothesized category
  probabilities. A chi-square statistic is calculated
  for each simulated sample to approximate the null
  distribution.

  The simulation p-value is calculated as

  \deqn{p = \frac{b+1}{B+1}}

  where b is the number of simulated chi-square statistics
  at least as large as the observed statistic, and
  B is the total number of simulations.

  Larger chi-square statistics provide stronger evidence
  against the hypothesized population distribution.

  The function does not require a significance level.
  The resulting p-value can be compared with a chosen
  significance level, such as 0.05.

  When plot = TRUE, a histogram of the simulated
  chi-square statistics is displayed, with extreme
  outcomes highlighted.
}

\value{
  Simulation-based goodness-of-fit test results,
  including the observed Pearson chi-square statistic,
  simulation p-value, and number of extreme simulations.

  The result also provides information about the
  simulated null distribution.
}

\examples{
# Example 1: Testing equal category probabilities

observed <- c(30, 45, 25)

set.seed(123)

chisq_gof(
  observed = observed,
  reps = 1000
)

# Example 2: Testing specified probabilities

chisq_gof(
  observed = c(40, 35, 25),
  p0 = c(0.50, 0.30, 0.20),
  reps = 1000
)

# Example 3: Display the simulation histogram

chisq_gof(
  observed = c(30, 45, 25),
  reps = 1000,
  plot = TRUE
)

# Example 4: Access simulation results

result <- chisq_gof(
  observed = c(30, 45, 25),
  reps = 1000
)

str(result)
}
