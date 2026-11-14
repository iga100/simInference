test_that("two-proportion bootstrap CI works correctly", {
  
  # Create two groups
  group <- c(rep("A", 50), rep("B", 50))
  
  # Binary outcomes: Yes = success, No = failure
  x <- c(
    rep("Yes", 30), rep("No", 20),
    rep("Yes", 20), rep("No", 30)
  )
  
  set.seed(123)
  
  result <- bootstrap_two_prop_ci(
    x = x,
    group = group,
    success = "Yes",
    group1 = "A",
    group2 = "B",
    reps = 1000,
    conf.level = 0.95,
    ci.method = "percentile"
  )
  
  # Check the result is a list
  expect_true(is.list(result))
  
  # Check the observed difference in proportions
  observed_diff <- mean(x[group == "A"] == "Yes") -
    mean(x[group == "B"] == "Yes")
  
  expect_equal(observed_diff, 0.20, tolerance = 1e-10)
  
  # Check that the confidence interval exists
  expect_true(is.data.frame(result$ci))
  
  # Check the confidence interval method
  expect_equal(result$ci$Method, "percentile")
  
  # Check that both endpoints exist
  expect_length(c(result$ci$Lower, result$ci$Upper), 2)
  
  # Check that the endpoints are finite and ordered
  expect_true(
    all(is.finite(c(result$ci$Lower, result$ci$Upper))) &&
      result$ci$Lower <= result$ci$Upper
  )
  
  # A difference in proportions must lie between -1 and 1
  expect_true(
    result$ci$Lower >= -1 &&
      result$ci$Upper <= 1
  )
  
})