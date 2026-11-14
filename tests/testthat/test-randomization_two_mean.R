test_that("two-mean randomization test works correctly", {
  
  # Create two groups
  group <- c(rep("A", 10), rep("B", 10))
  
  # Numerical observations
  x <- c(
    12, 14, 15, 16, 18, 20, 21, 22, 24, 25,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19
  )
  
  set.seed(123)
  
  result <- randomization_two_mean_test(
    x = x,
    group = group,
    group1 = "A",
    group2 = "B",
    alternative = "two.sided",
    reps = 1000
  )
  
  # Check that the result is a list
  expect_true(is.list(result))
  
  # Check that the p-value is numeric
  expect_true(is.numeric(result$p_value))
  
  # Check that the p-value is a single number
  expect_length(result$p_value, 1)
  
  # Check that the p-value is between 0 and 1
  expect_true(
    result$p_value >= 0 &&
      result$p_value <= 1
  )
  
})