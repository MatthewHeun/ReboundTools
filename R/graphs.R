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
rebound_graphs <- function(.path_data, arrow = NULL) {   # arrow = ggplot2::arrow(angle = 20, type = "closed")) {
  .path_data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_segment(mapping = ggplot2::aes(colour = colour, size = size, linetype = linetype,
                                                 x = x, y = y, xend = xend, yend = yend), 
                          arrow = arrow) +
    ggplot2::scale_colour_identity() + 
    ggplot2::scale_size_identity() + 
    ggplot2::scale_linetype_identity()
}





