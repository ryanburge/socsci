
# socsci: Tidyverse-Friendly Tools for Survey Analysis

**Weighted crosstabs, confidence intervals, and factor recoding — all in a few keystrokes.**

## Author

Ryan Burge — <https://www.ryanburge.net>
Reference site: <https://ryanburge.github.io/socsci/>

Professor of Practice, Washington University in St. Louis

------------------------------------------------------------------------

## Installation

``` r
install.packages("devtools")
devtools::install_github("ryanburge/socsci")
```

## Core Functions

| Function | Purpose |
|----------|---------|
| `ct()` | Count & tabulate with percentages (weighted or unweighted) |
| `mean_ci()` | Mean with confidence interval (weighted, Kish n\_eff) |
| `frcode()` | Recode via `case_when()` and keep factor level order |
| `corr()` | Correlation test with tidy output |
| `mean_med()` | Quick mean and median |
| `bind_df()` | Row-bind data frames matching a name pattern |
| `xbar()` | Crosstab stacked bar chart |
| `xheat()` | Crosstab heatmap |


## Counting Things with `ct()`

A weighted replacement for `janitor::tabyl()`. Respects existing `group_by()`, supports NA filtering, sorting, and cumulative totals.

``` r
library(socsci)

df <- tibble::tibble(
  race   = c("White", "Black", "Hispanic", "Asian", NA, "White", "Black"),
  weight = c(1, 1.2, 0.8, 1.5, 1, 1, 0.9)
)

# Unweighted
df %>% ct(race)
#> # A tibble: 5 x 3
#>   race         n   pct
#>   <chr>    <int> <dbl>
#> 1 Asian        1 0.143
#> 2 Black        2 0.286
#> 3 Hispanic     1 0.143
#> 4 White        2 0.286
#> 5 <NA>         1 0.143

# Weighted
df %>% ct(race, wt = weight)
#> # A tibble: 5 x 3
#>   race         n   pct
#>   <chr>    <dbl> <dbl>
#> 1 Asian      1.5 0.203
#> 2 Black      2.1 0.284
#> 3 Hispanic   0.8 0.108
#> 4 White      2   0.27
#> 5 <NA>       1   0.135

# Exclude NAs before computing pct
df %>% ct(race, show_na = FALSE)
#> # A tibble: 4 x 3
#>   race         n   pct
#>   <chr>    <int> <dbl>
#> 1 Asian        1 0.167
#> 2 Black        2 0.333
#> 3 Hispanic     1 0.167
#> 4 White        2 0.333

# Cumulative totals (sorted by frequency)
df %>% ct(race, wt = weight, show_na = FALSE, cum = TRUE, sort = TRUE)
#> # A tibble: 4 x 5
#>   race         n   pct cum_n cum_pct
#>   <chr>    <dbl> <dbl> <dbl>   <dbl>
#> 1 Black      2.1 0.328   2.1   0.328
#> 2 White      2   0.312   4.1   0.64
#> 3 Asian      1.5 0.234   5.6   0.874
#> 4 Hispanic   0.8 0.125   6.4   0.999
```

`ct()` respects existing groups, so within-group percentages just work:

``` r
df_grouped <- tibble::tibble(
  region = c("South", "South", "South", "North", "North", "North"),
  party  = c("Dem", "Rep", "Dem", "Dem", "Rep", "Rep")
)

df_grouped %>%
  dplyr::group_by(region) %>%
  ct(party)
#> # A tibble: 4 x 4
#> # Groups:   region [2]
#>   region party     n   pct
#>   <chr>  <chr> <int> <dbl>
#> 1 North  Dem       1 0.333
#> 2 North  Rep       2 0.667
#> 3 South  Dem       2 0.667
#> 4 South  Rep       1 0.333
```


## Confidence Intervals with `mean_ci()`

Computes mean, SD, standard error, and CI bounds. Supports survey weights (Kish effective sample size) and both t and normal critical values.

``` r
set.seed(1)
df2 <- tibble::tibble(
  x  = c(rnorm(8, 10, 2), NA_real_),
  w  = c(1, 2, 1, 1, 2, 1, 1, 3, 1)
)

# Unweighted, 95% CI (t critical)
mean_ci(df2, x)
#> # A tibble: 1 x 8
#>    mean    sd     n n_eff    se lower upper    ci
#>   <dbl> <dbl> <int> <int> <dbl> <dbl> <dbl> <dbl>
#> 1  10.3  1.71     8     8 0.603  8.84  11.7  0.95

# Weighted, 95% CI
mean_ci(df2, x, wt = w)
#> # A tibble: 1 x 8
#>    mean    sd     n n_eff    se lower upper    ci
#>   <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  10.5  1.50     8  6.55 0.585  9.05  12.0  0.95

# Unweighted, 90% CI (normal critical)
mean_ci(df2, x, ci = 0.90, dist = "normal")
#> # A tibble: 1 x 8
#>    mean    sd     n n_eff    se lower upper    ci
#>   <dbl> <dbl> <int> <int> <dbl> <dbl> <dbl> <dbl>
#> 1  10.3  1.71     8     8 0.603  9.27  11.3   0.9
```

Works with `group_by()` for easy comparisons:

``` r
mtcars %>%
  dplyr::group_by(cyl) %>%
  mean_ci(mpg)
#> # A tibble: 3 x 9
#>     cyl  mean    sd     n n_eff    se lower upper    ci
#>   <dbl> <dbl> <dbl> <int> <int> <dbl> <dbl> <dbl> <dbl>
#> 1     4  26.7  4.51    11    11 1.36   23.6  29.7  0.95
#> 2     6  19.7  1.45     7     7 0.549  18.4  21.1  0.95
#> 3     8  15.1  2.56    14    14 0.684  13.6  16.6  0.95
```


## Recoding with `frcode()`

Wraps `dplyr::case_when()` but returns a factor with levels in the order you write them — no more alphabetical sorting messing up your plots.

``` r
df3 <- tibble::tibble(
  pid7 = c(1,2,3,4,5,6,7, NA, 4, 2)
)

df3 %>%
  dplyr::mutate(
    pid_new = frcode(
      pid7 == 1 ~ "Strong Democrat",
      pid7 == 2 ~ "Not Strong Democrat",
      pid7 == 3 ~ "Lean Democrat",
      pid7 == 4 ~ "Independent",
      pid7 == 5 ~ "Lean Republican",
      pid7 == 6 ~ "Not Strong Republican",
      pid7 == 7 ~ "Strong Republican",
      TRUE ~ NA_character_
    )
  ) %>%
  ct(pid_new, show_na = FALSE)
#> # A tibble: 7 x 3
#>   pid_new                   n   pct
#>   <fct>                 <int> <dbl>
#> 1 Strong Democrat           1 0.111
#> 2 Not Strong Democrat       2 0.222
#> 3 Lean Democrat             1 0.111
#> 4 Independent               2 0.222
#> 5 Lean Republican           1 0.111
#> 6 Not Strong Republican     1 0.111
#> 7 Strong Republican         1 0.111
```


## Correlation with `corr()`

Tidy one-liner for Pearson correlation with p-value and confidence interval.

``` r
mtcars %>% corr(mpg, wt)
#> # A tibble: 1 x 7
#>   estimate statistic  p.value conf.low conf.high method                          n
#>      <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <chr>                       <int>
#> 1   -0.868     -9.56 1.29e-10   -0.934    -0.744 Pearson's product-moment…      32
```


## Mean & Median with `mean_med()`

Quick summary when you just need the center.

``` r
mtcars %>% mean_med(mpg)
#>       mean median
#> 1 20.09062   19.2
```


## Visualization: `xbar()` and `xheat()`

Quick crosstab plots (requires `ggplot2` and `scales`):

``` r
# Stacked bar chart
mtcars %>% xbar(cyl, am)

# Heatmap with counts
mtcars %>% xheat(cyl, am, count = TRUE)
```
