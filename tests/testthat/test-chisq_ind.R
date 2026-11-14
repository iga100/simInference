test_that("chi-square independence simulation works correctly", {
  
  # Observed frequencies in a 2 x 2 table
  observed <- matrix(
    c(30, 20, 15, 35),
    nrow = 2,
    byrow = TRUE
  )
  
  rownames(observed) <- c("Male", "Female")
  colnames(observed) <- c("Yes", "No")
  
  set.seed(123)
  
  result <- chisq_ind(
    x = observed,
    reps = 1000,
    plot = FALSE
  )
  
  # Check the result is a list
  expect_true(is.list(result))
  
  # Check the p-value exists and is numeric
  expect_true(is.numeric(result$p_value))
  
  # Check that the p-value is a single number
  expect_length(result$p_value, 1)
  
  # Check that the p-value is valid
  expect_true(
    is.finite(result$p_value) &&
      result$p_value >= 0 &&
      result$p_value <= 1
  )
  
})