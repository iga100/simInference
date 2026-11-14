test_that("two-proportion randomization test works correctly", {
  
  # Create two groups
  group <- c(rep("A", 50), rep("B", 50))
  
  # Binary outcomes: 1 = success, 0 = failure
  x <- c(
    rep(1, 30), rep(0, 20),
    rep(1, 20), rep(0, 30)
  )
  
  set.seed(123)
  
  result <- randomization_two_prop_test(
    x = x,
    group = group,
    success = 1,
    group1 = "A",
    group2 = "B",
    alternative = "two.sided",
    reps = 1000
  )
  
  # Check the result is a list
  expect_true(is.list(result))
  
  # Check that the p-value is numeric
  expect_true(is.numeric(result$p_value))
  
  # Check that the p-value is between 0 and 1
  expect_true(
    result$p_value >= 0 &&
      result$p_value <= 1
  )
  
  # Check that the p-value is a single number
  expect_length(result$p_value, 1)
  
})