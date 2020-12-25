#' Path to sample EEU data
#' 
#' Sample energy efficiency upgrade (EEU) data gives examples and enables testing.
#' This function returns a string path to an Excel file containing 
#' example data from the paper ---------------------.
#' Two energy efficiency upgrade examples are provided: a car and a light.
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


#' Load EED data from a sheet in an Excel workbook
#' 
#' Energy efficiency upgrade (EEU) data are often stored in an Excel workbook.
#' This function reads those data.
#'
#' @param path 
#' @param sheet 
#'
#' @return
#' @export
#'
#' @examples
load_eeu_data <- function(path = sample_eeu_data_path(), 
                          sheet = ReboundTools::eeu_data_table$eeu_data_sheet, 
                          expected_cols = ReboundTools::eeu_base_data) {
  readxl::read_excel(path, sheet = sheet)
}