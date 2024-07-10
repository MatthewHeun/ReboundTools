#' Path to sample EEU data
#' 
#' Sample energy efficiency upgrade (EEU) data gives examples and enables testing.
#' This function returns a string path to an Excel file containing 
#' example data from the paper ---------------------.
#' Two energy efficiency upgrade examples are provided: a car and an electric lamp.
#' 
#' Users can provide their own EEU files in similar format to
#' make their own estimates of rebound effects.
#'
#' @return A path to an example data file.
#' 
#' @export
#'
#' @examples
#' sample_eeu_data_path()
sample_eeu_data_path <- function() {
  file.path("extdata", "example_eeu_data.xlsx") %>% 
    system.file(package = "ReboundTools")
}


#' Load EEU data from a sheet in an Excel workbook
#' 
#' Energy efficiency upgrade (EEU) data are often stored in an Excel workbook.
#' This function reads those data and confirms that all required columns are present.
#' If not all columns are present, an error is thrown.
#'
#' @param path The path to the sample data spreadsheet. Default is `sample_eeu_data_path()`.
#' @param sheet The name of the tab in the sample data spreadsheet. See `ReboundTools::eeu_data_table`.
#' @param expected_col_names Names of columns that must be present in the target worksheet.
#'
#' @return A data frame of EEU data.
#' 
#' @export
#'
#' @examples
#' load_eeu_data()
load_eeu_data <- function(path = sample_eeu_data_path(), 
                          sheet = ReboundTools::eeu_data_table$eeu_data_sheet, 
                          expected_col_names = ReboundTools::eeu_base_params) {
  eeu_data <- readxl::read_excel(path, sheet = sheet)
  # Grab column names. Make sure each expected_col_name is present.
  cols_present <- which(expected_col_names %in% colnames(eeu_data))
  assertthat::assert_that(all(cols_present))
  return(eeu_data)
}