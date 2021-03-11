#' Create rebound graphs
#'
#' @param .analysis_data Rebound analysis data, likely created by `rebound_analysis()`.
#' @param indexed A boolean that tells whether to index the graph to its initial path point. Default is `FALSE`.
#' @param cases A string list saying which cases in `.rebound_data` to include. Default is `.analysis_data[[case_colname]] %>% unique()`, i.e. all cases in `.analysis_data`.
#' @param graph_types A string list of graph types to include in the returned object. Default is `ReboundTools::graph_types`, i.e. all graph types.
#' @param grid_types A string list of graph types on which grids (guide lines) are to be included. Default is `ReboundTools::graph_types`, i.e. include grids on all graph types.
#' @param graph_params A string list of parameters that control the appearance of this graph. 
#'                     Default is `ReboundTools::path_graph_params`, which can be 
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
#'   path_graphs(indexed = TRUE)
path_graphs <- function(.analysis_data,
                        indexed = FALSE,
                        cases = .analysis_data[[case_colname]] %>% unique(),
                        graph_types = ReboundTools::graph_types,
                        grid_types = ReboundTools::graph_types,
                        graph_params = ReboundTools::path_graph_params, 
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
        ggplot2::xlab(expression(dot(E)[dir] / dot(E)[dir]^degree * " [-]")) + 
        # Vertical axis label is E_dot_indir/E_dot_indir_orig
        ggplot2::ylab(expression(dot(E)[indir] / dot(E)[indir]^degree * " [-]"))
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
        ggplot2::xlab(expression(dot(C)[dir] / dot(C)[dir]^degree * " [-]")) + 
        # Vertical axis label is E_dot_indir/E_dot_indir_orig
        ggplot2::ylab(expression(dot(C)[indir] / dot(C)[indir]^degree * " [-]"))
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
      ggplot2::xlab(expression(dot(q)[s] / dot(q)[s]^degree * " [-]")) + 
      # Vertical axis label is C_dot_o/C_dot_o_orig
      ggplot2::ylab(expression(dot(C)[o] / dot(C)[o]^degree * " [-]"))
  } 
  
  return(g)
}


#' Create path maps for rebound analysis
#' 
#' This is a helper function for `path_graphs()`.
#' There is normally no need to call this function.
#' 
#' @param .path_data A data frame of paths to be added to the graph. 
#'                   The columns "colour" and "size" control the colour and width of the segment
#' @param .points_data A data frame of points between rebound effects.
#' @param .grid_data A data frame of lines to be added to the graph.
#' @param .indifference_data A data frame of indifference curves to be added to the graph.
#' @param graph_params A list of appearance parameters for this graph. Default is `ReboundTools::path_graph_params`.
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
                                  graph_params = ReboundTools::path_graph_params,
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


#' Sensitivity graphs for rebound analyses
#' 
#' A function to make sensitivity graphs for rebound analysis.
#' 
#' The caller can adjust the aesthetics of the graph with manual scales.
#'
#' @param .parametric_data A data frame, likely the result of calling `parametric_analysis()`.
#'                         Default is `parametric_analysis(rebound_data, parameterization)`.
#' @param rebound_data Rebound data, likely read by `load_eeu_data()`.
#'                     Default is `NULL`.
#' @param parameterization A list of lists that gives parameter sweeps.
#'                         At the top level, the list items must be named for cases in `rebound_data`.
#'                         At the next level, the parameters to be swept should be given, 
#'                         along with their sweep values. 
#'                         See examples.
#'                         Default is `NULL`.
#' @param x_var,y_var Strings that identify the x-axis and y-axis variables for this sensitivity graph.
#'                    These variables must appear in `.parametric_data`.
#'                    `x_var` must be a single string.
#'                    `y_var` can be a vector of strings. 
#'                    See examples.
#' @param line_var The name of variable to be used to discriminate lines on the graph. 
#'                 Default is `y_names_col`.
#' @param graph_params A list of parameters to control graph appearance. 
#'                     See `ReboundTools::sens_graph_params`.
#' @param y_names_col,y_vals_col See `ReboundTools::graph_df_colnames`.
#' @param point_type_colname,sweep_points,orig_points See `ReboundTools::parametric_analysis_point_types`.
#' 
#' @return A `ggplot2` object.
#' 
#' @export
#'
#' @examples
#' # Sensitivity of total rebound (Re_tot) to productivity multiplier (k)
#' df <- load_eeu_data()
#' sens_params <- list(Car = list(k = seq(0.5, 1.5, by = 0.5)), 
#'                     Lamp = list(k = seq(0, 2, by = 1)))
#' sensitivity_graphs(rebound_data = df, parameterization = sens_params, 
#'                    x_var = "k", y_var = "Re_tot") +
#'  ggplot2::facet_wrap(facets = "Case", scales = "free_x") +
#'  ggplot2::scale_colour_manual(values = c(Re_tot = "black"), guide = FALSE) + 
#'  ggplot2::scale_size_manual(values = c(Re_tot = 0.5), guide = FALSE) + 
#'  ggplot2::scale_linetype_manual(values = c(Re_tot = "solid"), guide = FALSE) +
#'  ggplot2::labs(y = expression(Re[tot]), 
#'                colour = ggplot2::element_blank(),
#'                size = ggplot2::element_blank(),
#'                linetype = ggplot2::element_blank())
#'                
#' # A more-complicated example that shows multi-variate sensitivity.
#' # Values of the productivity parameter (k) is shown in rows of the lattice plot.
#' # Uncompensated price elasticity of energy service consumption (e_qs_ps_UC) 
#' # is shown in columns of the lattice plot.
#' # Total rebound (Re_tot) is given on the y-axis, and 
#' # energy intensity of the economy (I_E) is given on the x-axis.
#' # The cases (Car and Lamp) are shown as different lines.
#' sens_params_2 <- list(Car = list(k = seq(0, 2, by = 0.5), 
#'                                I_E = seq(2, 5, by = 1), 
#'                                e_qs_ps_UC = seq(-0.5, -0.1, by = 0.1)), 
#'                     Lamp = list(k = seq(0, 2, by = 0.5),
#'                                 I_E = seq(2, 5, by = 1), 
#'                                 e_qs_ps_UC = seq(-0.5, -0.1, by = 0.1)))
#' sensitivity_graphs(rebound_data = df, parameterization = sens_params_2, 
#'                    x_var = "I_E", y_var = "Re_tot", line_var = "Case") +
#'   ggplot2::facet_grid(rows = ggplot2::vars(k), 
#'                       cols = ggplot2::vars(e_qs_ps_UC), scales = "free_y") +
#'   ggplot2::scale_colour_manual(values = c(Car = "black", Lamp = "red")) + 
#'   ggplot2::scale_size_manual(values = c(Car = 0.5, Lamp = 1.0)) + 
#'   ggplot2::scale_linetype_manual(values = c(Car = "solid", Lamp = "dashed")) + 
#'   ggplot2::labs(colour = ggplot2::element_blank(), 
#'                 size = ggplot2::element_blank(),
#'                 linetype = ggplot2::element_blank())
#'
#' # Plot all rebound terms as a function of post-upgrade efficiency
#' sens_params_3 <- list(Car = list(eta_engr_units_star = seq(35, 50, by = 0.5)), 
#'                       Lamp = list(eta_engr_units_star = seq(70, 90, by = 5)))
#' rebound_vars <- setdiff(ReboundTools::rebound_terms, ReboundTools::rebound_terms_agg) %>% 
#'   unlist()
#' sensitivity_graphs(rebound_data = df, parameterization = sens_params_3,
#'                    x_var = "eta_engr_units_tilde", 
#'                    y_var = rebound_vars) + 
#'   ggplot2::facet_wrap(facets = "Case", scales = "free_x") +
#'   ggplot2::scale_colour_manual(values = 
#'         c(Re_dempl = ReboundTools::path_graph_params$dempl_colour,
#'           Re_emb = ReboundTools::path_graph_params$emb_colour,
#'           Re_md = ReboundTools::path_graph_params$md_colour, 
#'           Re_dsub = ReboundTools::path_graph_params$dsub_colour,
#'           Re_isub = ReboundTools::path_graph_params$isub_colour, 
#'           Re_dinc = ReboundTools::path_graph_params$dinc_colour,
#'           Re_iinc = ReboundTools::path_graph_params$iinc_colour,
#'           Re_prod = ReboundTools::path_graph_params$prod_colour), 
#'                                breaks = rebound_vars) +
#'  ggplot2::scale_size_manual(values = 
#'         c(Re_dempl = 0.2, 
#'           Re_emb = ReboundTools::path_graph_params$emb_size,
#'           Re_md = ReboundTools::path_graph_params$md_size, 
#'           Re_dsub = ReboundTools::path_graph_params$dsub_size,
#'           Re_isub = ReboundTools::path_graph_params$isub_size, 
#'           Re_dinc = ReboundTools::path_graph_params$dinc_size,
#'           Re_iinc = ReboundTools::path_graph_params$iinc_size,
#'           Re_prod = ReboundTools::path_graph_params$prod_size), 
#'                             breaks = rebound_vars) +
#' ggplot2::scale_linetype_manual(values = 
#'         c(Re_dempl = ReboundTools::path_graph_params$dempl_linetype, 
#'           Re_emb = ReboundTools::path_graph_params$emb_linetype,
#'           Re_md = ReboundTools::path_graph_params$md_linetype,
#'           Re_dsub = ReboundTools::path_graph_params$dsub_linetype,
#'           Re_isub = "11",
#'           Re_dinc = ReboundTools::path_graph_params$dinc_linetype,
#'           Re_iinc = "11",
#'           Re_prod = ReboundTools::path_graph_params$prod_linetype), 
#'                                breaks = rebound_vars) +
#' ggplot2::labs(x = expression(tilde(eta)*" [mpg (Car) or lm/W (Lamp)]"), 
#'               y = "Re terms [-]", 
#'               colour = ggplot2::element_blank(),
#'               size = ggplot2::element_blank(),
#'               linetype = ggplot2::element_blank())
sensitivity_graphs <- function(.parametric_data = parametric_analysis(rebound_data, parameterization),
                               rebound_data = NULL, 
                               parameterization = NULL,
                               x_var,
                               y_var,
                               line_var = y_names_col,
                               y_names_col = ReboundTools::graph_df_colnames$y_names_col,
                               y_vals_col = ReboundTools::graph_df_colnames$y_vals_col,
                               graph_params = ReboundTools::sens_graph_params,
                               point_type_colname = ReboundTools::parametric_analysis_point_types$point_type_colname,
                               sweep_points = ReboundTools::parametric_analysis_point_types$sweep,
                               orig_points = ReboundTools::parametric_analysis_point_types$orig) {


  p_data <- .parametric_data %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(y_var), names_to = y_names_col, values_to = y_vals_col) %>% 
    # Arrange by the x variable so that all points (for geom_point) are in order.
    dplyr::arrange(.data[[x_var]])

  orig_data <- p_data %>%
    dplyr::filter(.data[[ReboundTools::parametric_analysis_point_types$point_type_colname]] == orig_points)
  line_data <- p_data %>%
    dplyr::filter(.data[[ReboundTools::parametric_analysis_point_types$point_type_colname]] == sweep_points)

  # Create the graph and return it
  ggplot2::ggplot() +
    ggplot2::geom_point(data = orig_data,
                        mapping = ggplot2::aes_string(x = x_var, y = y_vals_col),
                        colour = graph_params$orig_point_colour,
                        size = graph_params$orig_point_size,
                        shape = graph_params$orig_point_shape,
                        stroke = graph_params$orig_point_stroke,
                        fill = graph_params$orig_point_fill) +
    # Use geom_path so that we get nice rounded segments when using dashes.
    ggplot2::geom_path(data = line_data,
                       mapping = ggplot2::aes_string(x = x_var,
                                                     y = y_vals_col,
                                                     linetype = line_var,
                                                     colour = line_var,
                                                     size = line_var), 
                       lineend = graph_params$lineend, 
                       linejoin = graph_params$linejoin)
}


#' A sensitivity graph containing all rebound terms
#' 
#' Create a sensitivity graph with lines for all rebound terms. 
#' 
#' This function has the same arguments as `sensitivity_graphs()`,
#' except that `yvar` is missing.
#' `y_var` is missing, because the ordinate is assumed to be rebound values, 
#' and all rebound components are included.
#'
#' @param .parametric_data A data frame, likely the result of calling `parametric_analysis()`.
#'                         Default is `parametric_analysis(rebound_data, parameterization)`.
#' @param rebound_data Rebound data, likely read by `load_eeu_data()`.
#'                     Default is `NULL`.
#' @param parameterization A list of lists that gives parameter sweeps.
#'                         At the top level, the list items must be named for cases in `rebound_data`.
#'                         At the next level, the parameters to be swept should be given, 
#'                         along with their sweep values. 
#'                         See examples.
#'                         Default is `NULL`.
#' @param x_var Strings that identify the x-axis and y-axis variables for this sensitivity graph.
#'                    These variables must appear in `.parametric_data`.
#'                    `x_var` must be a single string.
#'                    `y_var` can be a vector of strings. 
#'                    See examples.
#' @param include_Re_tot A boolean that tells whether to include a line for total rebound.
#'                       Default is `TRUE`.
#' @param graph_params A list of parameters to control graph appearance. 
#'                     See `ReboundTools::sens_graph_params`.
#' @param point_type_colname,sweep_points,orig_points See `ReboundTools::parametric_analysis_point_types`.
#' @param line_var The name of variable to be used to discriminate lines on the graph. 
#'                 Default is `y_names_col`.
#' @param y_names_col,y_vals_col See `ReboundTools::graph_df_colnames`.
#' @param graph_params A list of parameters to control graph appearance. 
#'                     See `ReboundTools::sens_graph_params`.
#' @param point_type_colname,sweep_points,orig_points See `ReboundTools::parametric_analysis_point_types`.
#'
#' @return A ggplot2 graph showing sensitivity of all rebound terms to `x_var`.
#'  
#' @export
#'
#' @examples
#' df <- load_eeu_data()
#' sens_params <- list(Car = list(eta_engr_units_star = seq(35, 50, by = 0.5)), 
#'                     Lamp = list(eta_engr_units_star = seq(70, 90, by = 5)))
#' rebound_terms_graph(rebound_data = df, parameterization = sens_params, 
#'                     x_var = "eta_engr_units_tilde") +
#'   ggplot2::facet_wrap(facets = "Case", scales = "free_x")
rebound_terms_graph <- function(.parametric_data = parametric_analysis(rebound_data, parameterization),
                                rebound_data, 
                                parameterization,
                                x_var,
                                include_Re_tot = TRUE,
                                line_var = y_names_col,
                                y_names_col = ReboundTools::graph_df_colnames$y_names_col,
                                y_vals_col = ReboundTools::graph_df_colnames$y_vals_col,
                                graph_params = ReboundTools::sens_graph_params,
                                point_type_colname = ReboundTools::parametric_analysis_point_types$point_type_colname,
                                sweep_points = ReboundTools::parametric_analysis_point_types$sweep,
                                orig_points = ReboundTools::parametric_analysis_point_types$orig) {
  # Make a list of the rebound terms to graph.
  # These are the individual components.
  Re_vars_to_graph <- setdiff(ReboundTools::rebound_terms, ReboundTools::rebound_terms_agg)
  if (include_Re_tot) {
    Re_vars_to_graph <- append(Re_vars_to_graph, ReboundTools::rebound_terms$Re_tot)
  }
  # Swap indirect substitution and direct substitution effects to get a more-pleasing order.
  Re_vars_to_graph <- Re_vars_to_graph %>% 
    unlist() %>% 
    replace(c(4,5), Re_vars_to_graph[c(5,4)]) %>% 
    unlist() %>% 
    # Reverse to put total and productivity on the bottom
    rev()
  
  sensitivity_graphs(.parametric_data = .parametric_data, 
                     x_var = x_var, 
                     y_var = Re_vars_to_graph, 
                     line_var = line_var, 
                     y_vals_col = y_vals_col,
                     y_names_col = y_names_col,
                     graph_params = graph_params, 
                     point_type_colname = point_type_colname,
                     sweep_points = sweep_points,
                     orig_points = orig_points) +
    ggplot2::scale_colour_manual(values = c(Re_dempl = graph_params$dempl_colour,
                                            Re_emb = graph_params$emb_colour,
                                            Re_md = graph_params$md_colour, 
                                            Re_dsub = graph_params$dsub_colour,
                                            Re_isub = graph_params$isub_colour, 
                                            Re_dinc = graph_params$dinc_colour,
                                            Re_iinc = graph_params$iinc_colour,
                                            Re_prod = graph_params$prod_colour,
                                            Re_tot = graph_params$tot_colour), 
                                 breaks = Re_vars_to_graph) +
    ggplot2::scale_size_manual(values = c(Re_dempl = graph_params$dempl_size, 
                                          Re_emb = graph_params$emb_size,
                                          Re_md = graph_params$md_size, 
                                          Re_dsub = graph_params$dsub_size,
                                          Re_isub = graph_params$isub_size, 
                                          Re_dinc = graph_params$dinc_size,
                                          Re_iinc = graph_params$iinc_size,
                                          Re_prod = graph_params$prod_size, 
                                          Re_tot = graph_params$tot_size), 
                               breaks = Re_vars_to_graph) +
    ggplot2::scale_linetype_manual(values = c(Re_dempl = graph_params$dempl_linetype, 
                                              Re_emb = graph_params$emb_linetype,
                                              Re_md = graph_params$md_linetype,
                                              Re_dsub = graph_params$dsub_linetype,
                                              Re_isub = graph_params$isub_linetype,
                                              Re_dinc = graph_params$dinc_linetype,
                                              Re_iinc = graph_params$iinc_linetype,
                                              Re_prod = graph_params$prod_linetype, 
                                              Re_tot = graph_params$tot_linetype), 
                                   breaks = Re_vars_to_graph)
}

