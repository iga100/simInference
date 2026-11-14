test_that("one-mean bootstrap CI works correctly", {
  
  # Sample numerical data
  x <- c(12, 14, 15, 16, 18, 20, 22, 24)
  
  set.seed(123)
  
  result <- bootstrap_one_mean_ci(
    x = x,
    reps = 1000,
    conf.level = 0.95,
    ci.method = "percentile"
  )
  
  # Check the result is a list
  expect_true(is.list(result))
  
  # Check the observed sample mean
  expect_equal(mean(x), 17.625)
  
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
  
  # Check that the CI is within the sample range
  expect_true(
    result$ci$Lower >= min(x) &&
      result$ci$Upper <= max(x)
  )
  
})