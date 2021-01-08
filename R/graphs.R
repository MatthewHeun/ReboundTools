#' Create rebound graphs
#'
#' @param .rebound_data Rebound data, likely loaded by `load_eeu_data()`.
#' @param indexed A boolean that tells whether to index the graph to its initial path point. Default is `FALSE`.
#' @param cases A string list saying which cases in `.rebound_data` to include. Default is `.rebound_data[[case_colname]] %>% unique()`, i.e. all cases.
#' @param graph_types A string list of graph types to include in the returned object. Default is `ReboundTools::graph_types`, i.e. all graph types.
#' @param grid_types A string list of graph types on which grids (guide lines) are to be included. Default is `ReboundTools::graph_types`, i.e. include grids on all graph types.
#' @param graph_params A string list of parameters that control the appearance of this graph. 
#'                     Default is `ReboundTools::default_graph_params`, which can be 
#'                     modified and passed as an argument to control graph appearance.
#' @param case_colname The name of the Case column in `.rebound_data`. Default is `ReboundTools::eeu_base_params$case`.
#' @param graph_df_colnames The names of column names in data frames of graph data. Default is `ReboundTools::graph_df_colnames`.
#'  
#' @return A `ggplot2` object with the graphs
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_graphs()
rebound_graphs <- function(.rebound_data,
                           indexed = FALSE,
                           cases = .rebound_data[[case_colname]] %>% unique(),
                           graph_types = ReboundTools::graph_types,
                           grid_types = ReboundTools::graph_types,
                           graph_params = ReboundTools::default_graph_params, 
                           case_colname = ReboundTools::eeu_base_params$case, 
                           graph_df_colnames = ReboundTools::graph_df_colnames) {
  
  # Do the full rebound analysis
  analysis_data <- .rebound_data %>%
    dplyr::filter(.data[[case_colname]] %in% cases) %>% 
    rebound_analysis()
  
  # Calculate energy, cost, and preferences paths
  e_paths <- analysis_data %>%
    energy_paths(indexed = indexed, graph_params = graph_params)
  c_paths <- analysis_data %>% 
    cost_paths(indexed = indexed, graph_params = graph_params)
  p_paths <- analysis_data %>% 
    prefs_paths(graph_params = graph_params)
  # Bundle all paths together
  paths <- dplyr::bind_rows(e_paths, c_paths, p_paths) %>% 
    dplyr::filter(.data[[graph_df_colnames$graph_type_col]] %in% graph_types)

  # Calculate energy, cost, and preferences grids/guide lines
  e_grid_data <- analysis_data %>% 
    iso_energy_lines(indexed = indexed, graph_params = graph_params)
  c_grid_data <- analysis_data %>% 
    iso_cost_lines(indexed = indexed, graph_params = graph_params)
  p_grid_data <- analysis_data %>% 
    iso_budget_lines_prefs(graph_params = graph_params)
  # Decide which grids we want to keep.
  # I.e., we should not keep grids for graphs that we're not making.
  keep_grids <- intersect(graph_types, grid_types)
  # Bundle them together
  grids <- dplyr::bind_rows(e_grid_data, c_grid_data, p_grid_data) %>% 
    dplyr::filter(.data[[graph_df_colnames$graph_type_col]] %in% keep_grids)

  # Calculate indifference curves for the preferences graph  
  indifference_curves <- analysis_data %>% 
    indifference_lines(graph_params = graph_params) %>% 
    dplyr::filter(.data[[graph_df_colnames$graph_type_col]] %in% keep_grids)
  
  g <- rebound_graphs_helper(.path_data = paths, 
                        .grid_data = grids, 
                        .indifference_data = indifference_curves)
  
  # Now add x and y labels if possible, based on the type of graph.
  if (graph_types == ReboundTools::graph_types$energy) {
    # We know we have an energy graph.
    if (indexed) {
      g <- g +
        # Horizontal axis label E_dot_dir/E_dot_dir_orig
        ggplot2::xlab(expression(dot(E)[dir] / dot(E)[dir]^o * " [-]")) + 
        # Vertical axis label is E_dot_indir/E_dot_indir_orig
        ggplot2::ylab(expression(dot(E)[indir] / dot(E)[indir]^o * " [-]"))
    } else {
      g <- g +
        # Horizontal axis label E_dot_dir [MJ/year]
        ggplot2::xlab(expression(dot(E)[dir] * " [MJ/year]")) + 
        # Vertical axis label is E_dot_indir [MJ/year]
        ggplot2::ylab(expression(dot(E)[indir] * " [MJ/year]"))
    }
  }
  if (graph_types == ReboundTools::graph_types$cost) {
    if (indexed) {
      g <- g +
        # Horizontal axis label C_dot_dir/C_dot_dir_orig
        ggplot2::xlab(expression(dot(C)[dir] / dot(C)[dir]^o * " [-]")) + 
        # Vertical axis label is E_dot_indir/E_dot_indir_orig
        ggplot2::ylab(expression(dot(C)[indir] / dot(C)[indir]^o * " [-]"))
    } else {
      g <- g +
        # Horizontal axis label C_dot_dir [$/year]
        ggplot2::xlab(expression(dot(C)[dir] * " [$/year]")) + 
        # Vertical axis label is C_dot_indir [$/year]
        ggplot2::ylab(expression(dot(C)[indir] * " [$/year]"))
    }
  }
  if (graph_types == ReboundTools::graph_types$preferences) {
    # Preferences graphs are always indexed
    g <- g +
      # Horizontal axis label q_dot_s/q_dot_s_orig
      ggplot2::xlab(expression(dot(q)[s] / dot(q)[s]^o * " [-]")) + 
      # Vertical axis label is C_dot_o/C_dot_o_orig
      ggplot2::ylab(expression(dot(C)[o] / dot(C)[o]^o * " [-]"))
  } 
  
  return(g)
}


#' Create path maps for rebound analysis
#' 
#' This is a helper function for `rebound_graphs()`.
#' There is normally no need to call this function.
#' 
#' @param .path_data A data frame of paths to be added to the graph. 
#'                   The columns "colour" and "size" control the colour and width of the segment
#' @param .grid_data A data frame of lines to be added to the graph.
#' @param .indifference_data A data frame of indifference curves to be added to the graph.
#' @param graph_df_colnames The names of column names in data frames of graph data. Default is `ReboundTools::graph_df_colnames`.
#'
#' @return A `ggplot2` object with the graphs
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   energy_paths() %>% 
#'   rebound_graphs_helper()
rebound_graphs_helper <- function(.path_data, 
                                  .grid_data = NULL, 
                                  .indifference_data = NULL, 
                                  graph_df_colnames = ReboundTools::graph_df_colnames) {
  g <- ggplot2::ggplot()
  # Add grid data as first layer
  if (!is.null(.grid_data)) {
    g <- g + 
      ggplot2::geom_abline(data = .grid_data, 
                           mapping = ggplot2::aes_string(colour = graph_df_colnames$colour_col, 
                                                         size = graph_df_colnames$size_col, 
                                                         linetype = graph_df_colnames$linetype_col,
                                                         slope = graph_df_colnames$slope_col,
                                                         intercept =graph_df_colnames$ intercept_col))
  }
  # Add indifference curves as second layer
  if (!is.null(.indifference_data)) {
    g <- g + 
      ggplot2::geom_function(data = .indifference_data, 
                             mapping = ggplot2::aes_string(colour = graph_df_colnames$colour_col, 
                                                           size = graph_df_colnames$size_col, 
                                                           linetype = graph_df_colnames$linetype_col), 
                             fun = indifference_func, 
                             args = c(qs1_qs0 = .indifference_data$qs1_qs0, 
                                      Co1_Co0 = .indifference_data$Co1_Co0, 
                                      f_Cs_orig = .indifference_data$f_Cs_orig, 
                                      sigma = .indifference_data$sigma))
  }
  # Add rebound paths as third and final layer
  g +
    ggplot2::geom_segment(data = .path_data, 
                          mapping = ggplot2::aes_string(colour = graph_df_colnames$colour_col, 
                                                        size = graph_df_colnames$size_col,
                                                        linetype = graph_df_colnames$linetype_col,
                                                        x = graph_df_colnames$x_col, 
                                                        y = graph_df_colnames$y_col, 
                                                        xend = graph_df_colnames$xend_col, 
                                                        yend = graph_df_colnames$yend_col)) +
    # Use the colour, size, and linetype columns directly.
    ggplot2::scale_colour_identity() + 
    ggplot2::scale_size_identity() + 
    ggplot2::scale_linetype_identity()
}