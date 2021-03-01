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
#' `parameterization` must have structure where 
#' each element is named for a variable in `original_cases`, 
#' Values of each item in the list should be numerical values to be swept.
#' Note that the original value of each parameter (in `original_cases`)
#' will not be inserted into the `parameterization` lists,
#' so be sure to include that value, if desired.
#' `expand.grid()` is called on the entries of `parameterization`
#' to cover the union of all parameter values in the parametric study.
#'
#' @param original_cases A data frame with baseline data from which parametric studies are desired.
#'                       This data frame is expected to contain 1 row, a single case.
#' @param parameterization A named list. 
#' @param case_colname The name of the column of cases. Default is `ReboundTools::eeu_base_params$case`.
#'
#' @return A data frame containing results of parametric studies.
#' 
#' @export
#'
#' @examples
#' car_case <- load_eeu_data() %>% 
#'   dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Car")
#' params <- list(k = seq(0, 2, by = 1), p_E_engr_units = seq(1.5, 2.5, by = 0.25))
#' res <- parametric_studies(car_case, params)
#' dplyr::glimpse(res)
parametric_studies <- function(original_cases, parameterization, 
                               case_colname = ReboundTools::eeu_base_params$case) {
  
  # # Assert single row in original_cases
  # assertthat::assert_that(nrow(original_cases) == 1)
  # 
  # # Assert all item names in parameterization also appear in original_case
  # assertthat::assert_that(all(names(parameterization) %in% names(original_cases)))
  # 
  # # Make a new data frame of parameters to be used in the parametric analysis.
  # params <- expand.grid(parameterization)
  # 
  # # Make a data frame with lots of replicated rows, one for each row of params.
  # # Code adapted from https://stackoverflow.com/questions/8753531/repeat-rows-of-a-data-frame-n-times
  # original_cases[rep(seq_len(nrow(original_cases)), nrow(params)), ] %>% 
  #   # Eliminate columns that appear in params.
  #   # This step gets rid of columns that we now want to vary below.
  #   dplyr::select(-names(params)) %>% 
  #   # Bind params to cases to complete the data frame for analysis.
  #   dplyr::bind_cols(params) %>% 
  #   # Analyze the cases in the parametric study
  #   rebound_analysis()

  

  # Assert all cases in parameterization are in original_cases
  assertthat::assert_that(all(names(parameterization) %in% original_cases[[case_colname]]))  

  # Assert all item names in each case's parameterization 
  # also appear as column names in original_cases.
  lapply(parameterization, FUN = function(case) {
    assertthat::assert_that(all(names(case) %in% names(original_cases)))
  })
  
  # Make sure that varying parameters are same for all cases in parameterization
  
  
  # Set a variable that contains the varying parameters
  
  
  
  # Make a data frame of cases and parameter variations for each case
  per_case_parameters <- list()
  for (i in 1:length(parameterization)) {
    case_name <- names(parameterization)[[i]]
    case_params <- parameterization[[i]]
    per_case_parameters[[i]] <- expand.grid(case_params) %>% 
      dplyr::mutate(
        "{case_colname}" := case_name
      )
  }
  param_cases <- dplyr::bind_rows(per_case_parameters)
  
  original_cases %>% 
    # Eliminate the columns that will be parameterized
    dplyr::select(-names(parameterization)) %>% 
    dplyr::right_join(param_cases, by = case_colname) %>% 
    # Analyze the cases in the parametric study
    rebound_analysis()
  
  
  
}