#' Run parametric analyses
#' 
#' After defining a base case for rebound analyses,
#' parametric analyses and sensitivity studies are 
#' often helpful.
#' This function provides a way to do parametric studies.
#' 
#' `original_cases` must have the same structure as the example data frames in this package.
#' See the file at `sample_eeu_data_path()`.
#' 
#' `parameterization` must have structure where 
#' each element is named for a variable in `original_cases`, 
#' Values of each item in the list should be numerical values to be swept.
#' Note that the original value of each parameter (in `original_cases`)
#' will not be inserted into the `parameterization` lists,
#' so be sure to include that value, if desired.
#' If `parameterization` has more than 2 entries, 
#' `expand.grid()` is called on the entries
#' to cover the union of all parameter values.
#'
#' @param original_cases A data frame with baseline data from which parametric studies are desired.
#'                       This data frame is expected to contain 1 row, a single case.
#' @param parameterization A named list. 
#'
#' @return A data frame containing results of parametric studies.
#' 
#' @export
#'
#' @examples
#' 
parametric_studies <- function(original_cases, parameterization) {
  # Assert all cases in vars also appear in original_case
  
  
  
  # Assert all variables in vars also appear in original_case.
  
  
  # Make a new data frame of cases
  
  
  
  # Analyze the cases
  
  
  
  # Return the results
  
  
  
  
}