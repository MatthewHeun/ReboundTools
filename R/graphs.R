#' Title
#'
#' @param .rebound_data 
#'
#' @return
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   rebound_paths()
rebound_paths <- function(.rebound_data, 
                          indexed = FALSE,
                          I_E = ReboundTools::eeu_base_params$I_E,
                          E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
                          E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig,
                          C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                          C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                          S_dot_dev = ReboundTools::star_vars$S_dot_dev, 
                          C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig, 
                          C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig, 
                          G_dot = ReboundTools::star_vars$G_dot,
                          
                          S_dot_dev_colour = "red", 
                          S_dot_dev_linetype = "dashed",
                          
                          
                          energy_type = "Energy",
                          cost_type = "Cost", 
                          prefs_type = "Preferences") {
  
  # The strategy here is to make each segment individually, 
  # starting from the original point, and using Deltas for everything else.
 
  # A metadata data frame for all these segments
  meta <- .rebound_data %>% 
    dplyr::select(eeu_base_params$reference, eeu_base_params$case, 
                  eeu_base_params$original, eeu_base_params$upgrade)
  
  # S_dot_dev segment for energy graph
  x_orig_energy <- .rebound_data[[E_dot_s_orig]]
  y_orig_energy <- .rebound_data[[E_dot_emb_orig]] + 
    (.rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]) * .rebound_data[[I_E]]
  xend <- x_orig_energy - .rebound_data[[S_dot_dev]]
  yend <- y_orig_energy
  out <- add_segment(indexed = indexed,
                     colour = S_dot_dev_colour, linetype = S_dot_dev_linetype,
                     meta = meta, graph_type = energy_type, segment_name = "S_dot_dev", 
                     x_orig = x_orig_energy, y_orig = y_orig_energy,
                     x = x_orig_energy, y = y_orig_energy, xend = xend, yend = yend)
  
  # G_dot segment for cost graph
  x_orig_cost <- .rebound_data[[C_dot_s_orig]]
  y_orig_cost <- .rebound_data[[C_dot_cap_orig]] + .rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]
  xend <- x_orig_cost - .rebound_data[[G_dot]]
  yend <- y_orig_cost
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = S_dot_dev_colour, linetype = S_dot_dev_linetype,
                meta = meta, graph_type = cost_type, segment_name = "G_dot", 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x_orig_cost, y = y_orig_cost, xend = xend, yend = yend)
}


#' Add a line segment to a data frame
#'
#' @param .DF 
#' @param indexed 
#' @param meta 
#' @param graph_type 
#' @param segment_name 
#' @param colour 
#' @param linetype 
#' @param x_orig 
#' @param y_orig 
#' @param x 
#' @param y 
#' @param xend 
#' @param yend 
#'
#' @return A version of `.DF` with line segments added as the final row.
#' 
#' @export
#'
#' @examples
add_segment <- function(.DF = NULL, indexed, meta, graph_type, segment_name, 
                        colour = "black", linetype = "solid",
                        x_orig, y_orig,
                        x, y, xend, yend) {
  if (indexed) {
    x <- x/x_orig
    y <- y/y_orig 
    xend <- xend/x_orig
    yend <- yend/y_orig
  }
  out <- meta %>% 
    dplyr::mutate(
      graph_type = graph_type, 
      segment_name = segment_name,
      colour = colour, 
      linetype = linetype,
      x = x, 
      y = y, 
      xend = xend, 
      yend = yend
    )
  if (is.null(.DF)) {
    return(out)
  }
  .DF %>% 
    dplyr::bind_rows(out)
}




#' Create a path map for rebound analysis
#'
#' @param .path_data The paths to be graphed
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
rebound_graphs <- function(.path_data) {
  .path_data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_segment(mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend))
}