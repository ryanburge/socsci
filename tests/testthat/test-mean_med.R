test_that("mean_med() computes mean and median", {
  df <- data.frame(x = c(1, 2, 3, 4, 100))
  result <- mean_med(df, x)
  expect_equal(result$mean, 22)
  expect_equal(result$median, 3)
})

test_that("mean_med() handles NA with na.rm = TRUE (default)", {
  df <- data.frame(x = c(1, 2, 3, NA))
  result <- mean_med(df, x)
  expect_equal(result$mean, 2)
  expect_equal(result$median, 2)
})

test_that("mean_med() returns NA when na.rm = FALSE and NAs present", {
  df <- data.frame(x = c(1, 2, NA))
  result <- mean_med(df, x, na.rm = FALSE)
  expect_true(is.na(result$mean))
  expect_true(is.na(result$median))
})
