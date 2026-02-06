test_that("frcode() creates factor with correct level order", {
  df <- data.frame(x = c(1, 2, 3, 4))
  result <- dplyr::mutate(df,
    y = frcode(
      x == 1 ~ "First",
      x == 2 ~ "Second",
      x == 3 ~ "Third",
      TRUE ~ "Other"
    )
  )
  expect_s3_class(result$y, "factor")
  expect_equal(levels(result$y), c("First", "Second", "Third", "Other"))
})

test_that("frcode() preserves first-seen order for duplicate labels", {
  df <- data.frame(x = c(1, 2, 3))
  result <- dplyr::mutate(df,
    y = frcode(
      x <= 2 ~ "Low",
      x == 3 ~ "High",
      TRUE ~ "Low"
    )
  )
  expect_equal(levels(result$y), c("Low", "High"))
})

test_that("frcode() handles NA values", {
  df <- data.frame(x = c(1, 2, NA))
  result <- dplyr::mutate(df,
    y = frcode(
      x == 1 ~ "One",
      TRUE ~ NA_character_
    )
  )
  expect_equal(levels(result$y), "One")
  expect_true(is.na(result$y[2]))
  expect_true(is.na(result$y[3]))
})

test_that("frcode() errors with no arguments", {
  expect_error(frcode(), "requires at least one")
})

test_that("frcode() errors with non-formula argument", {
  expect_error(
    frcode("not a formula"),
    "must be two-sided formulas"
  )
})

test_that("frcode() warns on non-scalar RHS", {
  df <- data.frame(x = c(1, 2))
  expect_warning(
    dplyr::mutate(df, y = frcode(x == 1 ~ c("A", "B"), TRUE ~ "C")),
    "length > 1"
  )
})
