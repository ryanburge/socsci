test_that("mean_ci() computes unweighted mean correctly", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  result <- mean_ci(df, x)
  expect_equal(result$mean, 3)
  expect_equal(result$n, 5L)
  expect_equal(result$n_eff, 5L)
  expect_true(result$lower < result$mean)
  expect_true(result$upper > result$mean)
})

test_that("mean_ci() handles NA values with na.rm = TRUE", {
  df <- data.frame(x = c(1, 2, 3, NA, 5))
  result <- mean_ci(df, x, na.rm = TRUE)
  expect_equal(result$n, 4L)
  expect_false(is.na(result$mean))
})

test_that("mean_ci() computes weighted mean correctly", {
  df <- data.frame(x = c(1, 2), w = c(1, 3))
  result <- mean_ci(df, x, wt = w)
  # weighted mean: (1*1 + 2*3)/(1+3) = 7/4 = 1.75
  expect_equal(result$mean, 1.75)
  expect_equal(result$n, 2L)
})

test_that("mean_ci() returns consistent types for n and n_eff", {
  df <- data.frame(x = c(1, 2, 3))
  result <- mean_ci(df, x)
  expect_type(result$n, "integer")
  expect_type(result$n_eff, "integer")
})

test_that("mean_ci() handles empty data after NA removal", {
  df <- data.frame(x = c(NA_real_, NA_real_))
  result <- mean_ci(df, x, na.rm = TRUE)
  expect_equal(result$n, 0L)
  expect_true(is.na(result$mean))
})

test_that("mean_ci() handles n=1 without NA", {
  df <- data.frame(x = 42)
  result <- mean_ci(df, x)
  expect_equal(result$mean, 42)
  expect_equal(result$sd, 0)
  expect_equal(result$n, 1L)
})

test_that("mean_ci() uses normal distribution when requested", {
  df <- data.frame(x = c(1, 2, 3, 4, 5))
  result_t <- mean_ci(df, x, dist = "t")
  result_z <- mean_ci(df, x, dist = "normal")
  # t-distribution gives wider CIs than normal for small n
  expect_true((result_t$upper - result_t$lower) > (result_z$upper - result_z$lower))
})

test_that("mean_ci() respects groups", {
  df <- data.frame(g = c("a", "a", "b", "b"), x = c(1, 2, 10, 20))
  result <- mean_ci(dplyr::group_by(df, g), x)
  expect_equal(nrow(result), 2)
  expect_equal(result$mean, c(1.5, 15))
})

test_that("mean_ci() errors on non-numeric column", {
  df <- data.frame(x = c("a", "b"))
  expect_error(mean_ci(df, x), "numeric")
})

test_that("mean_ci() validates ci parameter", {
  df <- data.frame(x = 1:5)
  expect_error(mean_ci(df, x, ci = 1.5))
  expect_error(mean_ci(df, x, ci = 0))
  expect_error(mean_ci(df, x, ci = -0.5))
})
