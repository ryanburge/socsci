# socsci 2.0.0

* Major update with fully rewritten core functions:
  - `ct()` now supports sorting, cumulative columns, and weighted counts properly.
  - `mean_ci()` handles weighted/unweighted data robustly with correct n_eff.
  - `frcode()` preserves factor level order and works seamlessly in pipelines.
* Fixed printing and return-value bugs.
* Added more examples and README tests.
