#' Run parametric analyses
#' 
#' After defining a base case for rebound analyses,
#' parametric analyses and sensitivity studies are 
#' often helpful.
#' This function provides a convenient way to do parametric studies.
#' 
#' `original_cases` must have the same structure as the example data frames in this package.
#' See the file at `sample_eeu_data_path()`.
#' Failures will occur if `original_cases` contains columns calculated by `rebound_analysis()`.
#' 
#' `parameterization` is a list and must have the following structure. 
#' Top-level elements are named for cases.
#' Case names must match the case column in `original_cases`.
#' Not every case in `original_cases` must be present.
#' Each top-level element in `parameterization` must itself also be a list.
#' Each 2nd-level list must contain named vectors, with each name corresponding
#' to a parameter in `original_cases` that will be swept 
#' in the parametric study.
#' If more than one entry is included in a 2nd-level list, 
#' all combinations of values will be used, 
#' via `expand.grid()`.
#' Note that the original value of each parameter (in `original_cases`)
#' will not be inserted into the `parameterization` lists,
#' so be sure to include that value, if desired.
#' 
#' Note that the case names in `parameterization` (i.e., the names of the top-level entries)
#' are filtering toward `original_cases`. 
#' I.e., the output of this function will contain only those cases in `parameterization`.
#'
#' @param original_cases A data frame with baseline data from which parametric studies are desired.
#'                       This data frame is expected to contain 1 row, a single case.
#' @param parameterization A named list of lists. See details. 
#' @param include_orig_point A boolean that tells whether the original point for each case (from `original_cases`)
#'                           should be included in the output. Default is `TRUE`.
#' @param case_colname The name of the column of cases. Default is `ReboundTools::eeu_base_params$case`.
#'
#' @return A data frame containing results of parametric studies.
#' 
#' @export
#'
#' @examples
#' car_case <- load_eeu_data() %>% 
#'   dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Car")
#' # This example creates a parametric study for the "Car" case only,
#' # sweeping through values of both `k` and `p_E_engr_units`.
#' params <- list(Car = list(k = seq(0, 2, by = 1), 
#'                           p_E_engr_units = seq(1.5, 2.5, by = 0.25)))
#' res <- parametric_analysis(car_case, params)
#' dplyr::glimpse(res)
parametric_analysis <- function(original_cases, parameterization, 
                                include_orig_point = TRUE,
                                case_colname = ReboundTools::eeu_base_params$case, 
                                point_type_colname = ReboundTools::parametric_analysis_point_types$point_type_colname, 
                                orig_type = ReboundTools::parametric_analysis_point_types$orig,
                                sweep_type = ReboundTools::parametric_analysis_point_types$sweep) {
  
  cases <- names(parameterization)
  
  # Assert all cases in parameterization are in original_cases
  assertthat::assert_that(all(cases %in% original_cases[[case_colname]]))

  # Get a list of unique parameters for each case.
  params <- lapply(parameterization, FUN = function(case) {
    names(case)
  }) %>% 
    unique()
  # Make sure all of the parameters for each case are same.
  # If parameters for all cases are the same, 
  # the number of unique parameter lists will be 1.
  assertthat::assert_that(length(params) == 1)
  # We got a list of length 1, so unlist it
  # to create a vector of strings to be used later.
  params <- unlist(params)
  
  # Make sure that all params in parameterization
  # also appear as column names in original_cases.
  # This check guards against typos.
  assertthat::assert_that(all(params %in% names(original_cases)))
  
  # Build a data frame of all possible combinations of parameter values.
  param_combinations <- lapply(cases, FUN = function(case_name) {
    # Get the parameter values for this case
    parameterization[[case_name]] %>% 
      # Make a data frame out of all combinations of this case's parameters
      expand.grid() %>% 
      # Add the name of the case to the data frame of this case's parameters
      dplyr::mutate(
        "{case_colname}" := case_name
      )
  }) %>% 
    dplyr::bind_rows()
  
  # If we want to include the original point, 
  # create a data frame of rows of the original points.
  # Include one copy with orig_type (for points)
  # and one copy with sweep_type (for lines).
  orig_points <- dplyr::bind_rows(
    original_cases %>% dplyr::mutate("{point_type_colname}" := orig_type), 
    original_cases %>% dplyr::mutate("{point_type_colname}" := sweep_type)
  ) %>% 
    # Keep only those cases requeste in parameterization
    dplyr::filter(.data[[case_colname]] %in% cases)
  if (!include_orig_point) {
    # Empty the rows
    orig_points <- orig_points[-(1:nrow(orig_points)), ]
  }
  
  sweep_points <- original_cases %>% 
    # Focus only on the cases of interest
    dplyr::filter(.data[[case_colname]] %in% cases) %>% 
    # Eliminate the columns that are to be parameterized, leaving
    # only unparameterized variables
    dplyr::select(-dplyr::all_of(params)) %>% 
    # Join the parameterized variables in param_cases to the unparameterized variables
    dplyr::right_join(param_combinations, by = case_colname) %>% 
    # Add the point type column.
    dplyr::mutate(
      "{point_type_colname}" := sweep_type
    )
  
  # Bind the two data frames together, run the rebound analysis, and return the result.
  dplyr::bind_rows(orig_points, sweep_points) %>% 
    rebound_analysis()
}