test_that("one-proportion bootstrap CI works correctly", {
  
  # Sample data
  x <- c(rep("Yes", 60), rep("No", 40))
  
  set.seed(123)
  
  result <- bootstrap_one_prop_ci(
    x = x,
    success = "Yes",
    reps = 1000,
    conf.level = 0.95,
    ci.method = "percentile"
  )
  
  # Check the result is a list
  expect_true(is.list(result))
  
  # Check the observed sample proportion
  expect_equal(mean(x == "Yes"), 0.60)
  
  # Check the confidence interval exists
  expect_true(is.data.frame(result$ci))
  
  # Check the method
  expect_equal(result$ci$Method, "percentile")
  
  # Check the two confidence interval endpoints
  expect_length(c(result$ci$Lower, result$ci$Upper), 2)
  
  # Check that the bounds are valid
  expect_true(
    is.finite(result$ci$Lower) &&
      is.finite(result$ci$Upper) &&
      result$ci$Lower >= 0 &&
      result$ci$Upper <= 1 &&
      result$ci$Lower <= result$ci$Upper
  )
  
  # Independently verify the sample estimate
  if (!is.null(result$estimate)) {
    expect_equal(as.numeric(result$estimate), 0.60)
  }
  
})