
# SocSci: Functions for Analyzing Survey Data

## Author

Ryan Burge — <https://www.ryanburge.net>  
Reference site: <https://ryanburge.github.io/socsci/>

Professor of Practice, Washington University in St. Louis

------------------------------------------------------------------------

### Installation

You can install:

- the latest development version from GitHub with

  ``` r
  install.packages("devtools")
  devtools::install_github("ryanburge/socsci")
  ```

## There are just a handful of functions to the package right now

## Counting Things

I love the functionality of tabyl, but it doesn’t take a weight
variable. Here’s the simple version `ct()`

``` r
library(socsci)

df <- tibble::tibble(
  race   = c("White", "Black", "Hispanic", "Asian", NA, "White", "Black"),
  weight = c(1, 1.2, 0.8, 1.5, 1, 1, 0.9)
)

# Unweighted
df %>% ct(race)
#> # A tibble: 5 × 3
#>   race         n   pct
#>   <chr>    <int> <dbl>
#> 1 Asian        1 0.143
#> 2 Black        2 0.286
#> 3 Hispanic     1 0.143
#> 4 White        2 0.286
#> 5 <NA>         1 0.143
# Weighted
df %>% ct(race, wt = weight)
#> # A tibble: 5 × 3
#>   race         n   pct
#>   <chr>    <dbl> <dbl>
#> 1 Asian      1.5 0.203
#> 2 Black      2.1 0.284
#> 3 Hispanic   0.8 0.108
#> 4 White      2   0.27 
#> 5 <NA>       1   0.135
# Exclude NAs before computing pct
df %>% ct(race, show_na = FALSE)
#> # A tibble: 4 × 3
#>   race         n   pct
#>   <chr>    <int> <dbl>
#> 1 Asian        1 0.167
#> 2 Black        2 0.333
#> 3 Hispanic     1 0.167
#> 4 White        2 0.333
# Cumulative totals (sorted by frequency)
df %>% ct(race, wt = weight, show_na = FALSE, cum = TRUE, sort = TRUE)
#> # A tibble: 4 × 5
#>   race         n   pct cum_n cum_pct
#>   <chr>    <dbl> <dbl> <dbl>   <dbl>
#> 1 Black      2.1 0.328   2.1   0.328
#> 2 White      2   0.312   4.1   0.64 
#> 3 Asian      1.5 0.234   5.6   0.874
#> 4 Hispanic   0.8 0.125   6.4   0.999
```

## Getting Confidence Intervals

Oftentimes in social science we like to see what our 95% confidence
intervals are, but that’s a lot of syntax. It’s easy with the `mean_ci`
function.

``` r
set.seed(1)
df2 <- tibble::tibble(
  x  = c(rnorm(8, 10, 2), NA_real_),
  w  = c(1, 2, 1, 1, 2, 1, 1, 3, 1)
)

# Unweighted, 95% CI (t critical)
mean_ci(df2, x)
#> # A tibble: 1 × 8
#>    mean    sd     n n_eff    se lower upper    ci
#>   <dbl> <dbl> <int> <int> <dbl> <dbl> <dbl> <dbl>
#> 1  10.3  1.71     8     8 0.603  8.84  11.7  0.95

# Unweighted, 90% CI (normal critical)
mean_ci(df2, x, ci = 0.90, dist = "normal")
#> # A tibble: 1 × 8
#>    mean    sd     n n_eff    se lower upper    ci
#>   <dbl> <dbl> <int> <int> <dbl> <dbl> <dbl> <dbl>
#> 1  10.3  1.71     8     8 0.603  9.27  11.3   0.9

# Weighted, 95% CI
mean_ci(df2, x, wt = w)
#> # A tibble: 1 × 8
#>    mean    sd     n n_eff    se lower upper    ci
#>   <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  10.5  1.50     8  6.55 0.585  9.05  12.0  0.95

# Weighted, 84% CI (often used for visual comparison)
mean_ci(df2, x, wt = w, ci = 0.84)
#> # A tibble: 1 × 8
#>    mean    sd     n n_eff    se lower upper    ci
#>   <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  10.5  1.50     8  6.55 0.585  9.56  11.5  0.84
```

## Recode things and keep the factor levels

I recode all the time, but unfortunately when you recode from numeric to
character the factor levels are plotted in alphabetical order. There’s a
way around that now. This uses the `case_when` function from `dplyr` but
makes sure that the factors level are the same order of how they are
specified in the function.

``` r
df3 <- tibble::tibble(
  pid7 = c(1,2,3,4,5,6,7, NA, 4, 2)
)

df3 %>%
  mutate(
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
#> # A tibble: 7 × 3
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
