#' Build a stages table
#' 
#' A stages table has variables in rows and stages in columns.
#' These tables enable tracking of variables across the different stages of rebound.
#'
#' @param .analysis_data A data frame, usually the result of calling `rebound_analysis()`. Default is `rebound_analysis(load_eeu_data(file))`.
#' @param add_units When `TRUE` (the default), adds a unit specification to variable names in the table.
#' @param escape_latex When `TRUE` (the default), return LaTeX-compatible versions of strings.
#' @param vars A list of variables for rows of the table. Default is `ReboundTools::key_analysis_vars`.
#'             Variable order is preserved in the table.
#' @param latex_vars See `ReboundTools::latex_key_analysis_vars`. Set `NULL` to prevent conversion to LaTeX variable names.
#' @param stages A list of stages for columns of the table. Default is `ReboundTools::rebound_stages`.
#'               Stage order is preserved in the table.
#' @param latex_stages See `ReboundTools::latex_rebound_stages`. Set `NULL` to prevent conversion to LaTeX stage names.
#' @param case See `ReboundTools::eeu_base_params`.
#' @param service_unit,energy_engr_unit See `ReboundTools::eeu_base_params`.
#' @param ... Arguments passed to `xtable::xtable()`, possibly
#'            `label`, `caption`, `digits`, etc.
#' @param .var,.stage,.var_stage,.value,.name,.unit_col Column names used internally.
#'
#' @return An `xtable` object suitable for printing.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   stages_table()
stages_table <- function(.analysis_data, 
                         add_units = TRUE,
                         escape_latex = TRUE,
                         vars = ReboundTools::key_analysis_vars, 
                         latex_vars = ReboundTools::latex_key_analysis_vars,
                         stages = ReboundTools::rebound_stages, 
                         latex_stages = ReboundTools::latex_rebound_stages,
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
    # tidyr::pivot_wider(names_from = .data[[.stage]], values_from = .data[[.value]]) %>% 
    tidyr::pivot_wider(names_from = .stage, values_from = .value) %>% 
    dplyr::mutate(
      "{.unit_col}" := rebound_var_units(.var_name = .data[[.name]], 
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
        "{.name}" := dplyr::all_of(names(latex_vars)[[2]])
      ) %>% 
      # stages[[1]] is the first stage, usually "orig".
      dplyr::relocate(dplyr::all_of(.name), .before = stages[[1]])
  }
  # Now add the units to the variable name, if desired.
  if (add_units) {
    rebound_table_data <- rebound_table_data %>% 
      dplyr::mutate(
        "{.name}" := paste(.data[[.name]], .data[[.unit_col]]),
        "{.unit_col}" := NULL
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
      tidyr::pivot_longer(cols = unlist(unname(stages)), names_to = .stage, values_to = .value) %>% 
      dplyr::left_join(latex_stages, 
                       # (latex_vars))[[1]] is the name of the 
                       # first column in the latex_vars data frame.
                       # It is the column by which we want to join.
                       by = c(.stage = names(latex_stages)[[1]])) %>% 
      dplyr::mutate(
        "{.stage}" := NULL
      ) %>% 
      dplyr::rename(
        "{.stage}" := dplyr::all_of(names(latex_stages)[[2]])
      ) %>% 
      tidyr::pivot_wider(names_from = .stage, values_from = .value)
  }
  # Eliminate "name" title from name column. It looks stupid.
  rebound_table_data <- rebound_table_data %>% 
    dplyr::rename(
      ` ` = dplyr::all_of(.name)
    )
  
  # Create the xtable and return.
  rebound_table_data %>% 
    xtable::xtable(...)
}


#' Build a rebound results table 
#'
#' @param .analysis_data Rebound analysis results. Probably the output of a call to `rebound_analysis()`.
#' @param escape_latex When `TRUE` (the default), return LaTeX-compatible versions of strings.
#' @param include_subtotals Tells whether to include rebound subtotals 
#'                          by stage and direct/indirect.
#'                          Default is `TRUE`.
#' @param include_total Tells whether to include rebound total.
#'                          Default is `TRUE`.
#' @param as_percent When `TRUE`, rebound results are reported as percentages.
#'                   When `FALSE`, rebound results are reported as fractions.
#'                   Default is `TRUE`.
#' @param rebound_terms See `ReboundTools::rebound_terms`.
#' @param latex_rebound_terms See `ReboundTools::latex_rebound_terms`.
#' @param case See `ReboundTools::eeu_base_params`.
#' @param subtotals The rebound terms that represent subtotals. 
#'                  Default is `c(ReboundTools::rebound_terms$Re_empl, ReboundTools::rebound_terms$Re_sub, ReboundTools::rebound_terms$Re_inc, ReboundTools::rebound_terms$Re_dir, ReboundTools::rebound_terms$Re_indir, ReboundTools::rebound_terms$Re_micro)`.
#' @param total The rebound term that represents total rebound. Default is `ReboundTools::rebound_terms$Re_tot`.
#' @param term_name The title of the rebound term column. Default is "Rebound term".
#' @param Re_val_colname The title of the rebound value column. Default is "Value \[--\]".
#' @param perc_Re_val_colname The title of the rebound value column when percentages are requested. Default is "Value \[%\]".
#' @param latex_term_name The LaTeX term name column. Default is "LaTeX rebound term".
#' @param latex_Re_val_colname The LaTeX rebound value column name. Default is "Value \[--\]".
#' @param latex_perc_Re_val_colname The LaTeX rebound value column name when percentages are requested. Default is "Value \[\\%\]".
#' @param ... Arguments passed to `xtable::xtable()`, possibly
#'            `label`, `caption`, `digits`, etc.
#'
#' @return An `xtable` object suitable for printing.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   rebound_results_table()
rebound_results_table <- function(.analysis_data, 
                                  escape_latex = TRUE,
                                  include_subtotals = TRUE,
                                  include_total = TRUE,
                                  as_percent = TRUE,
                                  rebound_terms = ReboundTools::rebound_terms,
                                  latex_rebound_terms = ReboundTools::latex_rebound_terms,
                                  case = ReboundTools::eeu_base_params$case, 
                                  subtotals = c(ReboundTools::rebound_terms$Re_omd,
                                                ReboundTools::rebound_terms$Re_empl,
                                                ReboundTools::rebound_terms$Re_sub,
                                                ReboundTools::rebound_terms$Re_inc,
                                                ReboundTools::rebound_terms$Re_dir,
                                                ReboundTools::rebound_terms$Re_indir, 
                                                ReboundTools::rebound_terms$Re_micro),
                                  total = ReboundTools::rebound_terms$Re_tot,
                                  term_name = "Rebound term", 
                                  latex_term_name = "LaTeX rebound term",
                                  Re_val_colname = "Value [-]",
                                  perc_Re_val_colname = "Value [%]",
                                  latex_Re_val_colname = "Value [--]",
                                  latex_perc_Re_val_colname = "Value [\\%]",
                                  ...) {
  
  table_data <- .analysis_data %>% 
    dplyr::select(dplyr::any_of(case), 
                  dplyr::any_of(rebound_terms %>% unlist())) %>% 
    tidyr::pivot_longer(cols = unlist(unname(rebound_terms)),
                        names_to = term_name, 
                        values_to = Re_val_colname)
  if (!include_subtotals) {
    table_data <- table_data %>% 
      dplyr::filter(! .data[[term_name]] %in% subtotals)
  }
  if (!include_total) {
    table_data <- table_data %>% 
      dplyr::filter(! .data[[term_name]] %in% total)
  }
  if (as_percent) {
    table_data <- table_data %>% 
      dplyr::mutate(
        "{Re_val_colname}" := .data[[Re_val_colname]] * 100
      )
  }
  
  if (escape_latex) {
    # Change strings in .term_name column to be the LaTeX version.
    latex_names <- tibble::tibble("{term_name}" := names(latex_rebound_terms),
                                  "{latex_term_name}" := latex_rebound_terms %>% unlist())
    table_data <- dplyr::left_join(table_data, latex_names, by = term_name) %>%
      dplyr::mutate(
        "{term_name}" := NULL
      ) %>%
      dplyr::rename(
        "{term_name}" := dplyr::all_of(latex_term_name)
      ) %>%
      dplyr::relocate(dplyr::all_of(term_name), .before = dplyr::all_of(Re_val_colname))
  }
  
  # Adjust the value column name if needed.
  if (escape_latex & as_percent) {
    table_data <- table_data %>% 
      dplyr::rename(
        "{latex_perc_Re_val_colname}" := dplyr::all_of(Re_val_colname)
      )
  } else if (as_percent) {
    table_data <- table_data %>% 
      dplyr::rename(
        "{perc_Re_val_colname}" := dplyr::all_of(Re_val_colname)
      )
    
  }
  
  # Create the xtable and return.
  table_data %>% 
    xtable::xtable(...)
}
