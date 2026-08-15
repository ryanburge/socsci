test_that("endpoints() selects the last row in each series", {
  df <- tibble::tibble(
    year = rep(2020:2022, 2),
    series = rep(c("A", "B"), each = 3),
    value = 1:6
  )

  out <- endpoints(df, year, by = series)

  expect_equal(out$year, c(2022, 2022))
  expect_equal(out$series, c("A", "B"))
  expect_false(dplyr::is_grouped_df(out))
})

test_that("endpoints() supports first and both endpoints", {
  df <- tibble::tibble(year = 2020:2022, value = 1:3)

  expect_equal(endpoints(df, year, side = "first")$year, 2020)
  expect_equal(endpoints(df, year, side = "last")$year, 2022)
  expect_equal(endpoints(df, year, side = "both")$year, c(2020, 2022))
})

test_that("endpoints() honors and preserves existing groups", {
  df <- tidyr::expand_grid(
    facet = c("One", "Two"),
    series = c("A", "B"),
    year = 2020:2021
  ) %>%
    dplyr::group_by(facet)

  out <- endpoints(df, year, by = series)

  expect_equal(nrow(out), 4)
  expect_equal(dplyr::group_vars(out), "facet")
  expect_true(all(out$year == 2021))
})

test_that("endpoints() handles ties and missing x values", {
  df <- tibble::tibble(
    year = c(2020, 2021, 2021, NA),
    value = c("first", "tie one", "tie two", "missing")
  )

  expect_equal(nrow(endpoints(df, year)), 1)
  expect_equal(nrow(endpoints(df, year, with_ties = TRUE)), 2)
  expect_false(anyNA(endpoints(df, year)$year))
  expect_true(is.na(endpoints(df, year, na.rm = FALSE)$year[1]) ||
                endpoints(df, year, na.rm = FALSE)$year[1] == 2021)
})

test_that("endpoints() validates x and side", {
  df <- tibble::tibble(year = 2020:2022, value = 1:3)

  expect_error(endpoints(df, c(year, value)), "exactly one")
  expect_error(endpoints(df, year, side = "middle"), "arg")
})
