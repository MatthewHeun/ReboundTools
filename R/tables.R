#' Build a stages table
#' 
#' A stages table has variables in rows and stages in columns.
#' These tables enable tracking of variables across the different stages of rebound.
#'
#' @param .results A data frame, usually the result of calling `rebound_analysis()`. Default is `rebound_analysis(load_eeu_data(file))`.
#' @param vars A list of variables for rows of the table. Default is `ReboundTools::key_analysis_vars`.
#'             Variable order is preserved in the table.
#' @param latex_vars See `ReboundTools::latex_key_analysis_vars`. Set `NULL` to prevent conversion to LaTeX variable names.
#' @param stages A list of stages for columns of the table. Default is `ReboundTools::rebound_stages`.
#'               Stage order is preserved in the table.
#' @param latex_stages See `ReboundTools::latex_rebound_stages`. Set `NULL` to prevent conversion to LaTeX stage names.
#' @param file An optional path to a file. Default is `sample_eeu_data_path()`.
#' @param case See `ReboundTools::eeu_base_params`.
#' @param ... Arguments passed to `xtable::xtable()`, possibly
#'            `label`, `caption`, `digits`, etc.
#'
#' @return An `xtable` object giving the details of the table.
#' 
#' @export
#'
#' @examples
#' stages_table()
stages_table <- function(.results = rebound_analysis(load_eeu_data(file)), 
                         vars = ReboundTools::key_analysis_vars, 
                         latex_vars = ReboundTools::latex_key_analysis_vars,
                         stages = ReboundTools::rebound_stages, 
                         latex_stages = ReboundTools::latex_rebound_stages,
                         file = sample_eeu_data_path(), 
                         case = ReboundTools::eeu_base_params$case, 
                         ...) {
  
  # Build a data frame of all analysis variables.
  analysis_vars <- expand.grid(vars, stages) %>% 
    magrittr::set_names(c("var", "stage")) %>% 
    dplyr::mutate(
      vars_col = paste(var, stage, sep = "_"), 
      var = NULL, 
      stage = NULL
    ) %>% 
    unlist() %>% 
    unname()
  # Gather the data for the stages table.
  rebound_table_data <- .results %>% 
    dplyr::select(.data[[case]], all_of(analysis_vars)) %>% 
    tidyr::pivot_longer(cols = all_of(analysis_vars), names_to = "var_stage", values_to = "value") %>% 
    dplyr::mutate(
      name = sub(x = var_stage, pattern = "_[^_]*$", replacement = ""),
      name = factor(name, levels = vars),
      stage = sub(x = var_stage, pattern = ".*_", replacement = ""),
      stage = factor(stage, levels = stages),
      var_stage = NULL
    ) %>% 
    dplyr::arrange(.data[[case]], name, stage) %>% 
    tidyr::pivot_wider(names_from = stage, values_from = value)
    
  # Add LaTeX variable names, if not NULL.
  if (!is.null(latex_vars)) {
    rebound_table_data <- dplyr::left_join(rebound_table_data, latex_vars, by = c("name" = "var_name")) %>% 
      dplyr::mutate(
        name = NULL
      ) %>% 
      dplyr::rename(
        name = latex_var_name
      ) %>% 
      dplyr::relocate(name, .after = .data[[case]])
  }
  
  # Add LaTeX column names, if not NULL.
  if (!is.null(latex_stages)) {
    rebound_table_data <- rebound_table_data %>% 
      magrittr::set_names(c(case, "name", latex_stages$latex_stage_name))
  }
  
  # Create the xtable and return.
  rebound_table_data %>% 
    xtable::xtable(...)
}