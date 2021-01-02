#' Create a path map for rebound analysis
#'
#' @param .path_data A data frame of paths to be added to the graph. 
#'                   The columns "colour" and "size" control the colour and width of the segment
#'
#' @return A ggplot2 object with the graphs
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   rebound_paths() %>% 
#'   rebound_graphs()
rebound_graphs <- function(.path_data, 
                           .grid_data = NULL, 
                           .indifference_data = NULL, 
                           arrow = NULL) {   # arrow = ggplot2::arrow(angle = 20, type = "closed")) {
  g <- ggplot2::ggplot()
  # Add grid data as first layer
  if (!is.null(.grid_data)) {
    g <- g + 
      ggplot2::geom_abline(data = .grid_data, 
                           mapping = ggplot2::aes(colour = colour, size = size, linetype = linetype,
                                                  slope = slope, intercept = intercept))
  }
  # Add indifference curves as second layer
  if (!is.null(.indifference_data)) {
    g <- g + 
      ggplot2::geom_function(data = .indifference_data, 
                             mapping = ggplot2::aes(colour = colour, size = size, linetype = linetype), 
                             fun = indifference_func, 
                             args = c(qs1_qs0 = .indifference_data$qs1_qs0, 
                                      Co1_Co0 = .indifference_data$Co1_Co0, 
                                      f_Cs_orig = .indifference_data$f_Cs_orig, 
                                      sigma = .indifference_data$sigma))
  }
  # Add rebound paths as third and final layer
  g +
    ggplot2::geom_segment(data = .path_data, 
                          mapping = ggplot2::aes(colour = colour, size = size, linetype = linetype,
                                                 x = x, y = y, xend = xend, yend = yend), 
                          arrow = arrow) +
    # Use the colour, size, and linetype columns directly.
    ggplot2::scale_colour_identity() + 
    ggplot2::scale_size_identity() + 
    ggplot2::scale_linetype_identity()
}


