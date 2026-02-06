test_that("corr() returns correct columns", {
  df <- data.frame(x = 1:10, y = 1:10)
  result <- corr(df, x, y)
  expect_true(all(c("estimate", "statistic", "p.value", "conf.low", "conf.high", "n") %in% names(result)))
})

test_that("corr() computes perfect correlation", {
  df <- data.frame(x = 1:10, y = 1:10)
  result <- corr(df, x, y)
  expect_equal(result$estimate, 1)
  expect_equal(result$n, 10)
})

test_that("corr() handles NA values", {
  df <- data.frame(x = c(1, 2, 3, NA, 5), y = c(1, 2, 3, 4, 5))
  result <- corr(df, x, y, na.rm = TRUE)
  expect_equal(result$n, 4)
})
