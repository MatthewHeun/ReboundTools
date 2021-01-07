#' Create rebound graphs
#'
#' @param .rebound_data Rebound data, likely loaded by `load_eeu_data()`.
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
                           energy_path_graph = TRUE,
                           cost_path_graph = TRUE,
                           preferences_path_graph = TRUE, 
                           graph_params = ReboundTools::default_graph_params) {
  
  # analysis_data <- .rebound_data %>% 
  #   rebound_analysis()
  # e_paths <- analysis_data %>% 
  #   energy_paths(indexed = indexed, )
  # c_paths <- 
    
}




#' Create path maps for rebound analysis
#' 
#' This is a helper function for `rebound_graphs()`.
#' 
#' @param .path_data A data frame of paths to be added to the graph. 
#'                   The columns "colour" and "size" control the colour and width of the segment
#' @param .grid_data A data frame of lines to be added to the graph.
#' @param .indifference_data A data frame of indifference curves to be added to the graph.
#' @param colour_col,size_col,linetype_col,slope_col,intercept_col,x_col,y_col,xend_col,yend_col See `ReboundTools::graph_df_colnames`.
#'
#' @return A `ggplot2` object with the graphs
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   energy_paths() %>% 
#'   rebound_graphs()
rebound_graphs_helper <- function(.path_data, 
                                  .grid_data = NULL, 
                                  .indifference_data = NULL, 
                                  colour_col = ReboundTools::graph_df_colnames$colour_col, 
                                  size_col = ReboundTools::graph_df_colnames$size_col,
                                  linetype_col = ReboundTools::graph_df_colnames$linetype_col,
                                  slope_col = ReboundTools::graph_df_colnames$slope_col, 
                                  intercept_col = ReboundTools::graph_df_colnames$intercept_col, 
                                  x_col = ReboundTools::graph_df_colnames$x_col, 
                                  y_col = ReboundTools::graph_df_colnames$y_col, 
                                  xend_col = ReboundTools::graph_df_colnames$xend_col,
                                  yend_col = ReboundTools::graph_df_colnames$yend_col) {
  g <- ggplot2::ggplot()
  # Add grid data as first layer
  if (!is.null(.grid_data)) {
    g <- g + 
      ggplot2::geom_abline(data = .grid_data, 
                           mapping = ggplot2::aes_string(colour = colour_col, size = size_col, linetype = linetype_col,
                                                  slope = slope_col, intercept = intercept_col))
  }
  # Add indifference curves as second layer
  if (!is.null(.indifference_data)) {
    g <- g + 
      ggplot2::geom_function(data = .indifference_data, 
                             mapping = ggplot2::aes_string(colour = colour_col, size = size_col, linetype = linetype_col), 
                             fun = indifference_func, 
                             args = c(qs1_qs0 = .indifference_data$qs1_qs0, 
                                      Co1_Co0 = .indifference_data$Co1_Co0, 
                                      f_Cs_orig = .indifference_data$f_Cs_orig, 
                                      sigma = .indifference_data$sigma))
  }
  # Add rebound paths as third and final layer
  g +
    ggplot2::geom_segment(data = .path_data, 
                          mapping = ggplot2::aes_string(colour = colour_col, size = size_col, linetype = linetype_col,
                                                 x = x_col, y = y_col, xend = xend_col, yend = yend_col)) +
    # Use the colour, size, and linetype columns directly.
    ggplot2::scale_colour_identity() + 
    ggplot2::scale_size_identity() + 
    ggplot2::scale_linetype_identity()
}