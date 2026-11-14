test_that("one-proportion randomization test works", {
  
  x <- c(rep("Yes", 68), rep("No", 32))
  
  set.seed(123)
  
  result <- randomization_one_prop_test(
    x = x,
    success = "Yes",
    p0 = 0.60,
    alternative = "two.sided",
    reps = 1000
  )
  
  expect_equal(result$p_value, 0.1199, tolerance = 0.001)
  
})


