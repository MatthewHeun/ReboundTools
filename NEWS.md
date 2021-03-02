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
* Test coverage remains at 100%


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
