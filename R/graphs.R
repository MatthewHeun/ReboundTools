#' Create rebound graphs
#'
#' @param .analysis_data Rebound analysis data, likely created by `rebound_analysis()`.
#' @param indexed A boolean that tells whether to index the graph to its initial path point. Default is `FALSE`.
#' @param cases A string list saying which cases in `.rebound_data` to include. Default is `.rebound_data[[case_colname]] %>% unique()`, i.e. all cases.
#' @param graph_types A string list of graph types to include in the returned object. Default is `ReboundTools::graph_types`, i.e. all graph types.
#' @param grid_types A string list of graph types on which grids (guide lines) are to be included. Default is `ReboundTools::graph_types`, i.e. include grids on all graph types.
#' @param graph_params A string list of parameters that control the appearance of this graph. 
#'                     Default is `ReboundTools::default_graph_params`, which can be 
#'                     modified and passed as an argument to control graph appearance.
#' @param case_colname The name of the Case column in `.rebound_data`. Default is `ReboundTools::eeu_base_params$case`.
#' @param rebound_stages See `ReboundTools::rebound_stages`.
#' @param rebound_segments See `ReboundTools::rebound_segments`.
#' @param graph_df_colnames The names of column names in data frames of graph data. Default is `ReboundTools::graph_df_colnames`.
#'  
#' @return A `ggplot2` object with the graphs
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   rebound_graphs(indexed = TRUE)
rebound_graphs <- function(.analysis_data,
                           indexed = FALSE,
                           cases = .analysis_data[[case_colname]] %>% unique(),
                           graph_types = ReboundTools::graph_types,
                           grid_types = ReboundTools::graph_types,
                           graph_params = ReboundTools::default_graph_params, 
                           case_colname = ReboundTools::eeu_base_params$case, 
                           rebound_stages = ReboundTools::rebound_stages,
                           rebound_segments = ReboundTools::rebound_segments,
                           graph_df_colnames = ReboundTools::graph_df_colnames) {
  
  cases <- match.arg(cases, several.ok = TRUE)
  graph_types <- match.arg(unlist(graph_types), choices = unlist(graph_types), several.ok = TRUE)
  grid_types <- match.arg(unlist(grid_types), choices = unlist(grid_types), several.ok = TRUE)
  
  analysis_data <- .analysis_data %>%
    dplyr::filter(.data[[case_colname]] %in% cases)
  
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
  
  # Extract points between rebound effects
  e_points <- e_paths %>% 
    extract_points(graph_params = graph_params, 
                   rebound_stages = rebound_stages, 
                   rebound_segments = rebound_segments, 
                   graph_df_colnames = graph_df_colnames)
  c_points <- c_paths %>% 
    extract_points(graph_params = graph_params, 
                   rebound_stages = rebound_stages, 
                   rebound_segments = rebound_segments, 
                   graph_df_colnames = graph_df_colnames)
  p_points <- p_paths %>% 
    extract_points(graph_params = graph_params, 
                   rebound_stages = rebound_stages, 
                   rebound_segments = rebound_segments, 
                   graph_df_colnames = graph_df_colnames)
  # Bundle all points together
  points <- dplyr::bind_rows(e_points, c_points, p_points) %>% 
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
                             .points_data = points,
                             .grid_data = grids, 
                             .indifference_data = indifference_curves, 
                             graph_params = graph_params)
  
  # If the requested graph types has length > 1, we don't know what label to put on the axes,
  # so just return now.
  if (length(graph_types) > 1) {
    return(g)
  }
  
  # But if we have only 1 graph type, we can set the x and y axis labels.
  
  # Now add x and y labels if possible, based on the type of graph.
  if (graph_types == ReboundTools::graph_types$energy) {
    # We know we have an energy graph.
    if (indexed) {
      g <- g +
        # Horizontal axis label E_dot_dir/E_dot_dir_orig
        ggplot2::xlab(expression(dot(E)[dir] / dot(E)[dir]^scriptscriptstyle(o) * " [-]")) + 
        # Vertical axis label is E_dot_indir/E_dot_indir_orig
        ggplot2::ylab(expression(dot(E)[indir] / dot(E)[indir]^scriptscriptstyle(o) * " [-]"))
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
        ggplot2::xlab(expression(dot(C)[dir] / dot(C)[dir]^scriptscriptstyle(o) * " [-]")) + 
        # Vertical axis label is E_dot_indir/E_dot_indir_orig
        ggplot2::ylab(expression(dot(C)[indir] / dot(C)[indir]^scriptscriptstyle(o) * " [-]"))
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
      ggplot2::xlab(expression(dot(q)[s] / dot(q)[s]^scriptscriptstyle(o) * " [-]")) + 
      # Vertical axis label is C_dot_o/C_dot_o_orig
      ggplot2::ylab(expression(dot(C)[o] / dot(C)[o]^scriptscriptstyle(o) * " [-]"))
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
#' @param .points_data A data frame of points between rebound effects.
#' @param .grid_data A data frame of lines to be added to the graph.
#' @param .indifference_data A data frame of indifference curves to be added to the graph.
#' @param graph_params A list of appearance parameters for this graph. Default is `ReboundTools::default_graph_params`.
#' @param graph_types A list of graph types. Default is `ReboundTools::graph_types`.
#' @param graph_df_colnames The names of column names in data frames of graph data. Default is `ReboundTools::graph_df_colnames`.
#'
#' @return A `ggplot2` object containing graphs.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   energy_paths() %>% 
#'   rebound_graphs_helper()
rebound_graphs_helper <- function(.path_data, 
                                  .points_data = NULL,
                                  .grid_data = NULL, 
                                  .indifference_data = NULL, 
                                  graph_params = ReboundTools::default_graph_params,
                                  graph_types = ReboundTools::graph_types,
                                  graph_df_colnames = ReboundTools::graph_df_colnames) {
  # Set the order of the graph types via a factor. 
  .path_data <- .path_data %>% 
    dplyr::mutate(
      "{graph_df_colnames$graph_type_col}" := factor(.data[[graph_df_colnames$graph_type_col]], ReboundTools::graph_types)
    )
  if (!is.null(.points_data)) {
    .points_data <- .points_data %>% 
      # Only show points for which start_point_col is TRUE.
      dplyr::filter(.data[[graph_df_colnames$start_point_col]]) %>% 
      dplyr::mutate(
        "{graph_df_colnames$graph_type_col}" := factor(.data[[graph_df_colnames$graph_type_col]], ReboundTools::graph_types)
      )
  }
  if (!is.null(.grid_data)) {
    .grid_data <- .grid_data %>% 
      dplyr::mutate(
        "{graph_df_colnames$graph_type_col}" := factor(.data[[graph_df_colnames$graph_type_col]], ReboundTools::graph_types)
      )
  }
  if (!is.null(.indifference_data)) {
    .indifference_data <- .indifference_data %>% 
      dplyr::mutate(
        "{graph_df_colnames$graph_type_col}" := factor(.data[[graph_df_colnames$graph_type_col]], ReboundTools::graph_types)
      )
  }
  
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
  
  # Add indifference curve as a 2nd layer.
  if (!is.null(.indifference_data)) {
    g <- g + 
      ggplot2::geom_line(data = .indifference_data, 
                         mapping = ggplot2::aes_string(x = graph_df_colnames$x_col,
                                                       y = graph_df_colnames$y_col,
                                                       group = graph_df_colnames$line_name_col,
                                                       colour = graph_df_colnames$colour_col, 
                                                       size = graph_df_colnames$size_col, 
                                                       linetype = graph_df_colnames$linetype_col))
  }
  
  # If requested, add points between rebound effects as a third layer.
  if (!is.null(.points_data)) {
    if (! graph_params$points_atop_paths) {
      # Points are to be drawn beneath paths. 
      # So add points now.
      g <- g +
        ggplot2::geom_point(data = .points_data,
                            mapping = ggplot2::aes_string(x = graph_df_colnames$x_col,
                                                          y = graph_df_colnames$y_col,
                                                          shape = graph_df_colnames$shape_col,
                                                          size = graph_df_colnames$size_col,
                                                          fill = graph_df_colnames$fill_col,
                                                          stroke = graph_df_colnames$stroke_col,
                                                          colour = graph_df_colnames$colour_col))
    }
  }
  
  # Add rebound paths as fourth layer.
  # Use arrows, if requested.
  with_arrows <- .path_data %>% 
    dplyr::filter(.data[[graph_df_colnames$end_arrow_col]])
  without_arrows <- .path_data %>% 
    dplyr::filter(! .data[[graph_df_colnames$end_arrow_col]])
  # Segments without arrows
  g <- g +
    ggplot2::geom_segment(data = without_arrows, 
                          mapping = ggplot2::aes_string(colour = graph_df_colnames$colour_col, 
                                                        size = graph_df_colnames$size_col,
                                                        linetype = graph_df_colnames$linetype_col,
                                                        x = graph_df_colnames$x_col, 
                                                        y = graph_df_colnames$y_col, 
                                                        xend = graph_df_colnames$xend_col, 
                                                        yend = graph_df_colnames$yend_col), 
                          lineend = graph_params$lineend, 
                          linejoin = graph_params$linejoin)
  # Segments with arrows
  g <- g +
    ggplot2::geom_segment(data = with_arrows, 
                          mapping = ggplot2::aes_string(colour = graph_df_colnames$colour_col, 
                                                        size = graph_df_colnames$size_col,
                                                        linetype = graph_df_colnames$linetype_col,
                                                        x = graph_df_colnames$x_col, 
                                                        y = graph_df_colnames$y_col, 
                                                        xend = graph_df_colnames$xend_col, 
                                                        yend = graph_df_colnames$yend_col), 
                          lineend = graph_params$lineend, 
                          linejoin = graph_params$linejoin, 
                          # Here, we include the arrow.
                          arrow = graph_params$arrow_style)
  
  if (!is.null(.points_data)) {
    if (graph_params$points_atop_paths) {
      # Points are to be drawn atop paths. 
      # So add points as a final layer.
      g <- g +
        ggplot2::geom_point(data = .points_data,
                            mapping = ggplot2::aes_string(x = graph_df_colnames$x_col,
                                                          y = graph_df_colnames$y_col,
                                                          shape = graph_df_colnames$shape_col,
                                                          size = graph_df_colnames$size_col,
                                                          fill = graph_df_colnames$fill_col,
                                                          stroke = graph_df_colnames$stroke_col,
                                                          colour = graph_df_colnames$colour_col))
    }
  }
  
  g +  
    # Use the colour, size, linetype, and shape columns/data directly.
    ggplot2::scale_colour_identity() + 
    ggplot2::scale_size_identity() + 
    ggplot2::scale_linetype_identity() + 
    ggplot2::scale_shape_identity() + 
    ggplot2::scale_fill_identity()
}
