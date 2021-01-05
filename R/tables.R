#' Build a stages table
#' 
#' A stages table has variables in rows and stages in columns.
#' These tables enable tracking of variables across the different stages of rebound.
#'
#' @param .results A data frame, usually the result of calling `rebound_analysis()`. Default is `rebound_analysis(load_eeu_data(file))`.
#' @param vars A list of variables for rows of the table. Default is `ReboundTools::key_analysis_vars`.
#'             Variable order is preserved in the table.
#' @param add_units When `TRUE` (the default), adds a unit specification to variable names in the table.
#' @param latex_vars See `ReboundTools::latex_key_analysis_vars`. Set `NULL` to prevent conversion to LaTeX variable names.
#' @param stages A list of stages for columns of the table. Default is `ReboundTools::rebound_stages`.
#'               Stage order is preserved in the table.
#' @param latex_stages See `ReboundTools::latex_rebound_stages`. Set `NULL` to prevent conversion to LaTeX stage names.
#' @param file An optional path to a file. Default is `sample_eeu_data_path()`.
#' @param case See `ReboundTools::eeu_base_params`.
#' @param ... Arguments passed to `xtable::xtable()`, possibly
#'            `label`, `caption`, `digits`, etc.
#' @param .var,.stage,.var_stage,.value,.name,.unit_col Column names used internally.
#'
#' @return An `xtable` object giving the details of the table.
#' 
#' @export
#'
#' @examples
#' stages_table()
stages_table <- function(.analysis_data = rebound_analysis(load_eeu_data(file)), 
                         add_units = TRUE,
                         escape_latex = TRUE,
                         vars = ReboundTools::key_analysis_vars, 
                         latex_vars = ReboundTools::latex_key_analysis_vars,
                         stages = ReboundTools::rebound_stages, 
                         latex_stages = ReboundTools::latex_rebound_stages,
                         file = sample_eeu_data_path(), 
                         case = ReboundTools::eeu_base_params$case, 
                         service_unit = ReboundTools::eeu_base_params$service_unit,
                         energy_engr_unit = ReboundTools::eeu_base_params$energy_engr_unit,
                         ..., 
                         # internal names
                         .var = ".var",
                         .stage = ".stage", 
                         .var_stage = ".var_stage", 
                         .value = ".value", 
                         .name = ".name",
                         .unit_col = ".unit_col") {
  
  # Build a data frame of all analysis variables.
  analysis_vars <- expand.grid(vars, stages) %>% 
    magrittr::set_names(c(.var, .stage)) %>% 
    dplyr::mutate(
      vars_col = paste(.data[[.var]], .data[[.stage]], sep = "_"), 
      "{.var}" := NULL, 
      "{.stage}" := NULL
    ) %>% 
    unlist() %>% 
    unname()

  # Gather the data for the stages table.
  rebound_table_data <- .analysis_data %>% 
    dplyr::select(dplyr::any_of(case),
                  dplyr::all_of(analysis_vars), 
                  dplyr::any_of(service_unit), 
                  dplyr::any_of(energy_engr_unit)) %>% 
    tidyr::pivot_longer(cols = dplyr::all_of(analysis_vars), 
                        names_to = .var_stage, 
                        values_to = .value) %>% 
    dplyr::mutate(
      # Delete everything from the last "_" to the end of the string, inclusive.
      "{.name}" := sub(x = .data[[.var_stage]], pattern = "_[^_]*$", replacement = ""),
      "{.name}" := factor(.data[[.name]], levels = vars),
      # Delete everything from the start of the string to the last "_", inclusive.
      "{.stage}" := sub(x = .data[[.var_stage]], pattern = ".*_", replacement = ""),
      "{.stage}" := factor(.data[[.stage]], levels = stages),
      "{.var_stage}" := NULL
    ) %>% 
    dplyr::arrange() %>% 
    tidyr::pivot_wider(names_from = .data[[.stage]], values_from = .data[[.value]]) %>% 
    dplyr::mutate(
      "{.unit_col}" := units(.var_name = .data[[.name]], 
                       service_unit = .data[[service_unit]],
                       energy_engr_unit = .data[[energy_engr_unit]], 
                       escape_latex = escape_latex)
    )
    
  # Add LaTeX variable names, if not NULL.
  if (!is.null(latex_vars)) {
    rebound_table_data <- dplyr::left_join(rebound_table_data, latex_vars, 
                                           # names(latex_vars))[[1]] is the name of the 
                                           # first column in the latex_vars data frame.
                                           # It is the column by which we want to join.
                                           by = c(.name = names(latex_vars)[[1]]) ) %>% 
      dplyr::mutate(
        "{.name}" := NULL
      ) %>% 
      dplyr::rename(
        # names(latex_vars)[[2]] is the name of the column in latex_vars
        # that contains the LaTeX version of the names.
        "{.name}" := .data[[ names(latex_vars)[[2]] ]]
      ) %>% 
      # stages[[1]] is the first stage, usually "orig".
      dplyr::relocate(.data[[.name]], .before = stages[[1]])
  }
  # Now add the units to the variable name, if desired.
  if (add_units) {
    rebound_table_data <- rebound_table_data %>% 
      dplyr::mutate(
        "{.name}" := paste(.data[[.name]], .data[[.unit_col]]),
        "{.unit_col}" = NULL
      )
  }
  # At this point, we're done with the unit information, so delete those columns, if they exist.
  rebound_table_data <- rebound_table_data %>% 
    dplyr::mutate(
      "{service_unit}" := NULL,
      "{energy_engr_unit}" := NULL
    )
  
  # Add LaTeX column names, if not NULL.
  if (!is.null(latex_stages)) {
    rebound_table_data <- rebound_table_data %>% 
      tidyr::pivot_longer(cols = unlist(stages), names_to = .stage, values_to = .value) %>% 
      dplyr::left_join(latex_stages, 
                       # (latex_vars))[[1]] is the name of the 
                       # first column in the latex_vars data frame.
                       # It is the column by which we want to join.
                       by = c(.stage = names(latex_stages)[[1]])) %>% 
      dplyr::mutate(
        "{.stage}" := NULL
      ) %>% 
      dplyr::rename(
        "{.stage}" := .data[[ names(latex_stages)[[2]] ]]
      ) %>% 
      tidyr::pivot_wider(names_from = .stage, values_from = .value)
  }
  # Eliminate "name" title from name column. It looks stupid.
  rebound_table_data <- rebound_table_data %>% 
    dplyr::rename(
      ` ` = .data[[.name]]
    )
  
  # Create the xtable and return.
  rebound_table_data %>% 
    xtable::xtable(...)
}















