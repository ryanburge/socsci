# socsci 2.1.0

* New ggplot2 helpers for the `ct()`/`mean_ci()` plotting workflow:
  - `error_bar()` draws error bars from the `lower`/`upper` columns returned by `mean_ci()`.
  - `lab_bar()` adds percentage labels from a proportion column such as `ct()`'s `pct`,
    with options for placement, color, font family, and rounding.
  - `x_pct()` / `y_pct()` format a continuous axis as percentages.
  - `scale_fill_pid3()`, `scale_color_pid3()`, `scale_fill_pid7()`, `scale_color_pid7()`,
    `scale_fill_id5()`, and `scale_color_id5()` provide ordered party-ID and ideology
    palettes running blue (Democrat/liberal) to red (Republican/conservative), with a
    `reverse` argument.
* ggplot2 and scales moved from Suggests to Imports.

# socsci 2.0.0

* Major update with fully rewritten core functions:
  - `ct()` now supports sorting, cumulative columns, and weighted counts properly.
  - `mean_ci()` handles weighted/unweighted data robustly with correct n_eff.
  - `frcode()` preserves factor level order and works seamlessly in pipelines.
* Fixed printing and return-value bugs.
* Added more examples and README tests.
