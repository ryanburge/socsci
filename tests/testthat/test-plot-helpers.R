skip_if_not_installed("ggplot2")
skip_if_not_installed("scales")

test_that("error_bar() builds on mean_ci() output", {
  df <- data.frame(g = rep(c("a", "b"), each = 5), x = c(1:5, 6:10))
  ci <- mean_ci(dplyr::group_by(df, g), x)

  p <- ggplot2::ggplot(ci, ggplot2::aes(x = g, y = mean)) +
    ggplot2::geom_col() +
    error_bar()

  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[2]]$ymin, ci$lower)
  expect_equal(built$data[[2]]$ymax, ci$upper)
})

test_that("error_bar() passes width through", {
  layer <- error_bar(wd = 0.5)
  expect_s3_class(layer, "Layer")
  expect_equal(layer$geom_params$width, 0.5)
})

test_that("lab_bar() produces whole-percent labels from pct", {
  tab <- ct(data.frame(x = c("a", "a", "b", "b", "b")), x)

  p <- ggplot2::ggplot(tab, ggplot2::aes(x = x, y = pct)) +
    ggplot2::geom_col() +
    lab_bar(pct)

  built <- ggplot2::ggplot_build(p)
  expect_equal(built$data[[2]]$label, c("40%", "60%"))
})

test_that("lab_bar() respects pos, above, and color", {
  tab <- ct(data.frame(x = c("a", "a", "b", "b", "b")), x)

  p_above <- ggplot2::ggplot(tab, ggplot2::aes(x = x, y = pct)) +
    ggplot2::geom_col() +
    lab_bar(pct, pos = 0.05)
  built_above <- ggplot2::ggplot_build(p_above)
  expect_equal(built_above$data[[2]]$y, tab$pct + 0.05)

  p_fixed <- ggplot2::ggplot(tab, ggplot2::aes(x = x, y = pct)) +
    ggplot2::geom_col() +
    lab_bar(pct, pos = 0.1, above = FALSE, color = "white")
  built_fixed <- ggplot2::ggplot_build(p_fixed)
  expect_equal(built_fixed$data[[2]]$y, rep(0.1, 2))
  expect_equal(unique(built_fixed$data[[2]]$colour), "white")
})

test_that("x_pct() and y_pct() format labels as percentages", {
  xs <- x_pct()
  ys <- y_pct()
  expect_s3_class(xs, "ScaleContinuousPosition")
  expect_s3_class(ys, "ScaleContinuousPosition")
  expect_equal(xs$labels(c(0, 0.5, 1)), c("0%", "50%", "100%"))
  expect_equal(ys$labels(0.25), "25%")
})

test_that("pid scales have the right number of values and reverse correctly", {
  expect_length(scale_fill_pid3()$palette(3), 3)
  expect_length(scale_fill_pid7()$palette(7), 7)
  expect_length(scale_fill_id5()$palette(5), 5)

  fwd <- scale_fill_pid3()$palette(3)
  rev_ <- scale_fill_pid3(reverse = TRUE)$palette(3)
  expect_equal(rev_, rev(fwd))

  # Democrat/liberal end is blue, Republican/conservative end is red
  expect_equal(fwd[1], "dodgerblue3")
  expect_equal(fwd[3], "firebrick3")
  expect_equal(scale_fill_pid7()$palette(7)[1], "#000080")
  expect_equal(scale_fill_id5()$palette(5)[5], "#B2182B")
})

test_that("color and fill pid scales use the same palettes", {
  expect_equal(scale_color_pid3()$palette(3), scale_fill_pid3()$palette(3))
  expect_equal(scale_color_pid7()$palette(7), scale_fill_pid7()$palette(7))
  expect_equal(scale_color_id5()$palette(5), scale_fill_id5()$palette(5))
})
