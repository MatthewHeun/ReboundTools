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
