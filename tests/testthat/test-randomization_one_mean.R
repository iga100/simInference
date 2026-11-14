test_that("one-mean randomization test works correctly", {
  
  x <- c(12, 14, 15, 16, 18, 20, 22, 24)
  
  set.seed(123)
  
  result <- randomization_one_mean_test(
    x = x,
    mu0 = 15,
    alternative = "two.sided",
    reps = 1000
  )
  
  # Check that the function returns a result
  expect_true(is.list(result))
  
  # Check that the p-value exists
  expect_true(is.numeric(result$p_value))
  
  # Check that the p-value is valid
  expect_length(result$p_value, 1)
  expect_true(
    result$p_value >= 0 &&
      result$p_value <= 1
  )
  
})