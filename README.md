# simInference

**Simulation-Based Statistical Inference for Teaching**

`simInference` is an R package designed to support the teaching and learning of simulation-based statistical inference.

The package provides functions for randomization hypothesis tests, bootstrap confidence intervals, and chi-square simulation tests.

Functions support ordinary R vectors and data-frame column expressions, making the package suitable for classroom demonstrations, assignments, and introductory statistics courses.

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("iga100/simInference")
```

Load the package:

```r
library(simInference)
```

## Available Functions

### Randomization Hypothesis Tests

| Function | Description |
|---|---|
| `randomization_one_prop_test()` | Randomization test for one population proportion |
| `randomization_one_mean_test()` | Randomization test for one population mean |
| `randomization_two_prop_test()` | Randomization test for the difference between two proportions |
| `randomization_two_mean_test()` | Randomization test for the difference between two means |

### Bootstrap Confidence Intervals

| Function | Description |
|---|---|
| `bootstrap_one_prop_ci()` | Bootstrap confidence interval for one proportion |
| `bootstrap_one_mean_ci()` | Bootstrap confidence interval for one mean |
| `bootstrap_two_prop_ci()` | Bootstrap confidence interval for the difference between two proportions |
| `bootstrap_two_mean_ci()` | Bootstrap confidence interval for the difference between two means |

### Chi-Square Simulation Tests

| Function | Description |
|---|---|
| `chisq_gof()` | Chi-square goodness-of-fit test using simulation |
| `chisq_ind()` | Chi-square test of independence using simulation |

## Examples

### Example 1: Randomization Test for One Proportion

Suppose 68 out of 100 students prefer eating at the campus dining hall. We want to test whether the population proportion differs from 0.60.

```r
library(simInference)

x <- c(rep("Yes", 68), rep("No", 32))

set.seed(123)

randomization_one_prop_test(
  x = x,
  success = "Yes",
  p0 = 0.60,
  alternative = "two.sided",
  reps = 1000
)
```

### Example 2: Randomization Test for One Mean

```r
x <- c(12, 14, 15, 16, 18, 20, 22, 24)

set.seed(123)

randomization_one_mean_test(
  x = x,
  mu0 = 15,
  alternative = "two.sided",
  reps = 1000
)
```

### Example 3: Bootstrap Confidence Interval for One Proportion

```r
x <- c(rep("Yes", 60), rep("No", 40))

set.seed(123)

bootstrap_one_prop_ci(
  x = x,
  success = "Yes",
  reps = 1000,
  conf.level = 0.95,
  ci.method = "percentile"
)
```

### Example 4: Bootstrap Confidence Interval for Two Means

```r
group <- c(rep("A", 10), rep("B", 10))

x <- c(
  12, 14, 15, 16, 18, 20, 21, 22, 24, 25,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19
)

set.seed(123)

bootstrap_two_mean_ci(
  x = x,
  group = group,
  group1 = "A",
  group2 = "B",
  reps = 1000,
  conf.level = 0.95,
  ci.method = "percentile"
)
```

### Example 5: Chi-Square Test of Independence

```r
observed <- matrix(
  c(30, 20, 15, 35),
  nrow = 2,
  byrow = TRUE
)

rownames(observed) <- c("Male", "Female")
colnames(observed) <- c("Yes", "No")

set.seed(123)

chisq_ind(
  x = observed,
  reps = 1000,
  plot = FALSE
)
```

## Package Testing

Version 0.1.0 has completed its initial automated testing.

- All 10 exported functions have automated tests.
- 46 test assertions passed.
- `devtools::check()` reported 0 errors and 0 warnings.
- One environment-related note was reported because the current time could not be verified.

These checks establish initial functionality but do not constitute exhaustive statistical validation.

## Author

**Innocent Abaa**  


## License

MIT License

## Version

**0.1.0 — Initial GitHub Release**

The package is under active development and is not currently available on CRAN.