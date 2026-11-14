test_that("two-mean bootstrap CI works correctly", {
  
  # Create two groups
  group <- c(rep("A", 10), rep("B", 10))
  
  # Numerical observations
  x <- c(
    12, 14, 15, 16, 18, 20, 21, 22, 24, 25,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19
  )
  
  set.seed(123)
  
  result <- bootstrap_two_mean_ci(
    x = x,
    group = group,
    group1 = "A",
    group2 = "B",
    reps = 1000,
    conf.level = 0.95,
    ci.method = "percentile"
  )
  
  # Check the result is a list
  expect_true(is.list(result))
  
  # Check the observed difference in means
  observed_diff <- mean(x[group == "A"]) -
    mean(x[group == "B"])
  
  expect_equal(observed_diff, 4.2, tolerance = 1e-10)
  
  # Check the confidence interval exists
  expect_true(is.data.frame(result$ci))
  
  # Check the method
  expect_equal(result$ci$Method, "percentile")
  
  # Check that both endpoints exist
  expect_length(c(result$ci$Lower, result$ci$Upper), 2)
  
  # Check that the endpoints are finite and ordered
  expect_true(
    all(is.finite(c(result$ci$Lower, result$ci$Upper))) &&
      result$ci$Lower <= result$ci$Upper
  )
  
  # Check that the interval lies within possible
  # differences based on the observed sample ranges
  min_diff <- min(x[group == "A"]) - max(x[group == "B"])
  max_diff <- max(x[group == "A"]) - min(x[group == "B"])
  
  expect_true(
    result$ci$Lower >= min_diff &&
      result$ci$Upper <= max_diff
  )
  
})