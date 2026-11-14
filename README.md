# simInference

An R package for teaching simulation-based inference.

**Status:** Development version. R package checks have not yet been run in this environment.

## Installation from GitHub

```r
install.packages("remotes")
remotes::install_github("YOUR_USERNAME/simInference")
library(simInference)
```

## Examples

```r
set.seed(123)
d <- data.frame(Year = rep(c("FirstYear", "Senior"), each = 20),
                Sex = rep(c("F", "M"), 20),
                Score = rnorm(40))

chisq_ind(Year, Sex, data = d, reps = 1000, plot = FALSE)
chisq_ind(d$Year, d$Sex, reps = 1000, plot = FALSE)
bootstrap_one_mean_ci(Score, data = d, reps = 1000)
randomization_one_mean_test(Score, mu0 = 0, data = d, reps = 1000)
```

## Available functions

- `randomization_one_prop_test()`
- `randomization_one_mean_test()`
- `randomization_two_prop_test()`
- `randomization_two_mean_test()`
- `bootstrap_one_prop_ci()`
- `bootstrap_one_mean_ci()`
- `bootstrap_two_prop_ci()`
- `bootstrap_two_mean_ci()`
- `chisq_gof()`
- `chisq_ind()`

## Before publication

1. Replace the placeholder maintainer email in `DESCRIPTION`.
2. Run `devtools::check()` in RStudio and resolve any errors or warnings.
3. Run the interface test in `tests/interface.R`.
4. Review the statistical methods and improve the initial documentation.
