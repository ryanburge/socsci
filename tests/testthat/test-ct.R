test_that("ct() counts correctly without weights", {
  df <- data.frame(x = c("a", "a", "b", "b", "b"))
  result <- ct(df, x)
  expect_equal(nrow(result), 2)
  expect_equal(result$n, c(2, 3))
  expect_equal(result$pct, c(0.4, 0.6))
})

test_that("ct() handles weighted counts", {
  df <- data.frame(x = c("a", "b"), w = c(3, 7))
  result <- ct(df, x, w)
  expect_equal(result$n, c(3, 7))
  expect_equal(result$pct, c(0.3, 0.7))
})

test_that("ct() respects show_na = FALSE", {
  df <- data.frame(x = c("a", "b", NA))
  result_with <- ct(df, x, show_na = TRUE)
  result_without <- ct(df, x, show_na = FALSE)
  expect_equal(nrow(result_with), 3)
  expect_equal(nrow(result_without), 2)
})

test_that("ct() respects existing groups", {
  df <- data.frame(g = c("x", "x", "y", "y"), val = c("a", "b", "a", "a"))
  result <- ct(dplyr::group_by(df, g), val)
  # within group "x": a=1, b=1 -> 0.5, 0.5
  # within group "y": a=2 -> 1.0
  expect_equal(nrow(result), 3)
  x_rows <- result[result$g == "x", ]
  expect_equal(x_rows$pct, c(0.5, 0.5))
})

test_that("ct() sorts by descending count when sort = TRUE", {
  df <- data.frame(x = c("a", "b", "b", "b", "c", "c"))
  result <- ct(df, x, sort = TRUE)
  expect_equal(result$x, c("b", "c", "a"))
})

test_that("ct() adds cumulative columns when cum = TRUE", {
  df <- data.frame(x = c("a", "a", "b", "c"))
  result <- ct(df, x, cum = TRUE)
  expect_true("cum_n" %in% names(result))
  expect_true("cum_pct" %in% names(result))
  expect_equal(result$cum_n, c(2, 1 + 2, 1 + 1 + 2))
  expect_equal(result$cum_pct[3], 1.0)
})

test_that("ct() handles empty data frame", {
  df <- data.frame(x = character(0))
  result <- ct(df, x)
  expect_equal(nrow(result), 0)
  expect_true("pct" %in% names(result))
})

test_that("ct() errors on multi-column selection", {
  df <- data.frame(a = 1, b = 2)
  expect_error(ct(df, c(a, b)), "exactly one column")
})
