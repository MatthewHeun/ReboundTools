---
title: "Release notes for `ReboundTools`"
output: html_document
---


Cite all releases with doi [10.5281/zenodo.4999846](https://doi.org/10.5281/zenodo.4999846), 
which always resolves to the latest release.


* Added rows for R_alpha*C_dot_cap and 
  R_omega*C_dot_d 
  in stages table.
* Now propagating energy price (p_E)
  across all stages, despite the fact that it never changes.
* Additional tests for new features.
    - Now up to 708 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.39 (2024-05-15) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11199447.svg)](https://doi.org/10.5281/zenodo.11199447)

* Use new budget constraint with discounting.
* Additional tests for new features.
    - Now up to 695 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.38 (2023-07-05) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8117284.svg)](https://doi.org/10.5281/zenodo.8117284)

* Fixed a bug in `calc_orig()`.
  Some calculated parameters were in the argument list.


# ReboundTools 0.1.37 (2023-01-28) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7579832.svg)](https://doi.org/10.5281/zenodo.7579832)

* Update to latest versions of GitHub actions.
* Fix text failures caused by
  changes to the renaming policy in tidyr::pivot_longer().
  Renaming via named vectors is no longer allowed.
* No new tests.
    - Still at 612 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.36 (2023-01-08) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7514249.svg)](https://doi.org/10.5281/zenodo.7514249)

* Switch to "yr" instead of "year" for default time unit.


# ReboundTools 0.1.35 (2023-01-02) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7499769.svg)](https://doi.org/10.5281/zenodo.7499769)

* Simplified calculations at the hat stage.
* Fixed `linewidth`s in a few sensitivity graph examples.
* Now calculating elasticities 
  (e_qs_ps_C, e_qo_ps_C, e_qs_ps_UC, and e_qo_ps_UC) 
  at every stage
  (orig, star, hat, bar, tilde)
  for both the CPE utility model and the CES utility model.
* Deleted some old code dealing with linewidths.
* Additional tests for new features.
    - Now up to 612 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.34 (2022-12-10) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7423306.svg)](https://doi.org/10.5281/zenodo.7423306)

* Finish the task of responding to the new `linewidth` aesthetic
  in `ggplot2`, especially for sensitivity graphs
  and the vignettes.
* Macro rebound now tied to N_dot_star 
  (instead of N_dot_hat).
* Now using `ggplot2::scale_discrete_manual()` and 
  `ggplot2::scale_discrete_identity()` for 
  various rebound graphs, thereby 
  eliminating some warnings.
* No new tests.
    - Still at 472 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.33 (2022-11-20) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7340040.svg)](https://doi.org/10.5281/zenodo.7340040)

* Now using the `linewidth` aesthetic in `ggplot2` where possible.
* Responded to changes in `tidyselect` and `ggplot2`.
  Many functions were throwing deprecation warnings.
* No new tests.
    - Still at 472 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.32 (2021-06-20) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4999847.svg)](https://doi.org/10.5281/zenodo.4999847)

* Final release before minting a DOI and submitting paper.
* Changed lifecycle badge from experimental to stable in preparation for first submission of paper.
* No new tests.
    - Still at 472 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.31 (2021-05-31)

* Changed default for dashed lines in sensitivity plots from "11" to "22"
  to improve visibility.
* Moved to new formula for calculating M_dot_hat_prime:
  `M_dot_hat_prime_val <- M_dot_hat_val - C_dot_cap_star_val - C_dot_md_star_val - N_dot_hat_val`
* Added `N_dot` description for `key_analysis_vars` in `data.R`.
  Thanks to Zeke Marshall for noting it was missing.
* Eliminated last vestiges of preference path graphs.
* No new tests.
    - Still at 472 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.30 (2021-05-03)

* "preference path graphs" --> "consumption path graphs"
* No new tests.
    - Still at 472 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.29 (2021-04-29)

* Added isoquant lines through the * and ^ point
  to the grid created by the `iso_expenditure_lines()` function.
* No new tests.
    - Still at 472 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.28 (2021-04-26)

* Now calculating micro rebound.
* New test for the new feature.
    - Now at 472 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.27 (2021-04-23)

* Added item `show_indifference_curves` to `graph_params` item of `path_graphs()` function.
* New tests for the new argument.
    - Now at 470 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.26 (2021-04-22)

* Fixed a bug where missing meta columns would cause graphing failure.
* New tests to trigger the bug.
    - Now at 469 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.25 (2021-04-21)

* New function `calibrate_k()` that calculates a value for `k`
  when a target value for `Re_tot` is provided.
* New tests for the new function.
    - Now at 467 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.24 (2021-04-14)

* Change names around elasticities:
    - e_qs_ps --> e_qs_ps_C (for "compensated")
    - e_qo_ps --> e_qo_ps_C (for "compensated")
* A couple new tests.
    - Now at 460 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.23 (2021-04-09)

* Set up CodeCov from GitHub Actions.
* No new tests.
    - Still at 458 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.22 (2021-04-08)

* Remove Travis continuous integration.
* Add badges for GitHub actions and CodeCov.
* Add GitHub actions for continuous integration.
* No new tests.
    - Still at 458 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.21 (2021-04-07)

* Set up Travis continuous integration.
* No new tests.
    - Still at 458 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.20 (2021-04-06)

* Improved website landing page.
* Split graphs into their own vignette.
* Added a vignette section on sensitivity graphs.
* No new tests.
    - Still at 458 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.19 (2021-04-06)

* Change cost --> expenditure in many places.
* No new tests.
    - Still at 458 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.18 (2021-04-03)

* Change nomenclature from productivity rebound to macro rebound.
* No new tests.
    - Still at 458 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.17 (2021-03-21)

* Added rho to orig output.
* Two new tests.
    - Now at 458 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.16 (2021-03-18)

* Adjusted some graph parameters:
  line widths and dashes for `rebound_terms_graph()`
* Implemented `points_atop_paths` for `rebound_terms_graph()`.
  Default now set to `TRUE`.
* One new test.
    - Now at 456 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.15 (2021-03-15)

* Dots now take on colour of their lines in `sensitivity_graphs()`.
* New option to use LaTeX for legend labels in
  `rebound_terms_graph()`.
* New tests for new features.
    - Now at 455 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.14 (2021-03-14)

* Fixed a bug with line layering in 
  `sensitivity_graphs()` and `rebound_terms_graph()`.
* Now including `Re_cap` in rebound calculations.
* Renamed `Re_d` and `Re_i` to `Re_dir` and `Re_indir`.
* New tests for new features.
    - Now at 454 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.13 (2021-03-11)

* Many improvements to graphs.
* New function `rebound_terms_graph()`
  that makes graphs of all rebound terms.
* Re-implement `sensitivity_graphs()`.
* New tests for new features.
    - Now at 452 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.12 (2021-03-10)

* Removed `sensitivity_graphs()` for now.
* New constants for sensitivity graphs, `sens_graph_params`.
* Name changes: 
    * `default_graph_params` --> `path_graph_params`
    * `rebound_graphs()` --> `path_graphs()`.
* Added graph parameters for total rebound.
* New tests for new features.
    - Now at 447 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.11 (2021-03-02)

* New function `sensitivity_graphs()` makes graphs from parametric data.
* `parametric_analysis()` now optionally returns the original point,
  which may be helpful for graphing purposes.
  E.g., one could make a graph with lines for the swept parameters and 
  red dots for the original (base) points.
* Added a first version of the `parametric_analysis()` function.
* New tests for new features.
    - Now at 449 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.10 (2021-02-09)

* New colors for graphs, using the Viridis palette, plasma option.
* Now allowing an approximation for the substitution effect.
  New argument `use_sub_approx` triggers the approximation
  and is set `FALSE` in several places.
* Calculating hat point (after substitution effect)
  via new derivation that ensures expenditure line is tangent to indifference curve.
* Fixed a bug that swapped order of energy and cost path graphs.
* New tests for new features.
    - Now at 412 tests, all passing.
    - Test coverage remains at 100%.


# ReboundTools 0.1.9 (2021-01-17)

* Added efficiency in engineering units (e.g., mpg) to stages table.
* Now at 344 tests, all passing.
* Test coverage remains at 100%.


# ReboundTools 0.1.8 (2021-01-16)

* Fixed a bug where `Re_prod` was counted as a subtotal.
  It should not be.
* Now at 337 tests, all passing.
* Test coverage is at 100%


# ReboundTools 0.1.7 (2021-01-16)

* Added function `rebound_results_table()`.
* Added a vignette.
* Now at 336 tests, all passing.
* Test coverage is at 100%


# ReboundTools 0.1.6 (2021-01-10)

* Now at 314 tests, all passing.
* Test coverage is at 100%
* Preferences graph now contains self-generated indifference curves.


# ReboundTools 0.1.5 (2021-01-08)

* Now at 303 tests, all passing.
* Test coverage is 99.84 %. 
* New graph parameters:
    - `linend`
    - `linejoin`
    - `include_start_point`
    - `start_point_size`
    - `start_point_shape`
    - `include_end_arrow`
    - `arrow_angle`
    - `arrow_length`
    - `arrow_type`


# ReboundTools 0.1.4 (2021-01-07)

* Added x and y labels for graphs, when possible.
* Tests for graphs.
* Much refactoring.
    - Graph appearance parameters are now passed as a list.
    - Data frame column names now mostly stored in member lists.
* Fixed a bug in `stages_table()`. 
  `"{.unit_col}" = NULL` --> `"{.unit_col}" := NULL`
  

# ReboundTools 0.1.3 (2021-01-05)

* Still at 280 tests, all passing.
* Package test coverage is 99%
* Eliminated all errors and warnings.
* Added documentation for functions.


# ReboundTools 0.1.2 (2021-01-03)

* Now at 280 tests, all passing.
* New function `stages_table()` that produces an `xtable` for later printing.
* New function `units()` that determines units for each variable,
  using heuristics from the naming system.


# ReboundTools 0.1.1 (2021-01-02)

* Now at 246 tests, all passing.
* Test coverage is 99.78 %. 
  The only line of code not covered by tests
  requires two indifference curves to be added to the same graph.
  That line will be covered when more tests are added.
* Added functions to calculate indifference curves for preferences graph.
* Added functions to calculate iso lines on energy, cost, and preferences graphs.
* Added functions to calculate paths for rebound analysis on energy, cost, and preferences graphs.
* Added functions to calculate all rebound parameters.
* Added `sample_eeu_data_path()` function to give the path to an Excel file
  containing same energy efficiency upgrade data.
* Added a `NEWS.md` file to track changes to the package.
