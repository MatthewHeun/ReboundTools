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
                          S_dot_dev_colour = "red", 
                          S_dot_dev_size = 0.1,
                          
                          C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig, 
                          C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig, 
                          
                          G_dot = ReboundTools::star_vars$G_dot,
                          G_dot_colour = "red",
                          G_dot_size = 1,
                          
                          Delta_E_dot_emb_star = ReboundTools::Delta_vars$Delta_E_dot_emb_star,
                          Delta_E_dot_emb_star_colour = "red",
                          Delta_E_dot_emb_star_size = 1,
                          
                          Delta_C_dot_md_star = ReboundTools::Delta_vars$Delta_C_dot_md_star,
                          Delta_C_dot_md_star_I_E_colour = "black",
                          Delta_C_dot_md_star_I_E_size = 0.5,
                          Delta_C_dot_md_star_colour = "red", 
                          Delta_C_dot_md_star_size = 1,
                          
                          Delta_C_dot_cap_star = ReboundTools::Delta_vars$Delta_C_dot_cap_star,
                          Delta_C_dot_cap_star_colour = "black", 
                          Delta_C_dot_cap_star_size = 0.5,
                          
                          Delta_E_dot_s_hat = ReboundTools::Delta_vars$Delta_E_dot_s_hat,
                          Delta_E_dot_s_hat_colour = "orange",
                          Delta_E_dot_s_hat_size = 1,

                          Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                          Delta_C_dot_o_hat_I_E_colour = "orange",
                          Delta_C_dot_o_hat_I_E_size = 1,
                          
                          Delta_E_dot_s_bar = ReboundTools::Delta_vars$Delta_E_dot_s_bar,
                          Delta_E_dot_s_bar_colour = "darkgreen",
                          Delta_E_dot_s_bar_size = 0.5,
                          
                          Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar,
                          Delta_C_dot_o_bar_I_E_colour = "darkgreen",
                          Delta_C_dot_o_bar_I_E_size = 0.5,
                          
                          k = ReboundTools::eeu_base_params$k,
                          N_dot_hat = ReboundTools::hat_vars$N_dot_hat,
                          prod_colour = "darkblue", 
                          prod_size = 1,
                          
                          Delta_C_dot_s_hat = ReboundTools::Delta_vars$Delta_C_dot_s_hat,
                          Delta_C_dot_s_hat_colour = "orange",
                          Delta_C_dot_s_hat_size = 1,
                          
                          Delta_C_dot_o_hat_colour = "orange",
                          Delta_C_dot_o_hat_size = 1,
                          
                          Delta_C_dot_s_bar = ReboundTools::Delta_vars$Delta_C_dot_s_bar,
                          Delta_C_dot_s_bar_colour = "darkgreen",
                          Delta_C_dot_s_bar_size = 0.5,

                          Delta_C_dot_o_bar_colour = "darkgreen",
                          Delta_C_dot_o_bar_size = 0.5,
                          
                          energy_type = "Energy",
                          cost_type = "Cost", 
                          prefs_type = "Preferences") {
  
  # The strategy here is to make each segment individually, 
  # starting from the original point, and using Deltas for everything else.
 
  # A metadata data frame for all these segments
  meta <- .rebound_data %>% 
    dplyr::select(eeu_base_params$reference, eeu_base_params$case, 
                  eeu_base_params$original, eeu_base_params$upgrade)
  
  #
  # Start of Energy graph
  # 
  
  # Emplacement effect
  
  # S_dot_dev segment for energy graph
  x_orig_energy <- .rebound_data[[E_dot_s_orig]]
  y_orig_energy <- .rebound_data[[E_dot_emb_orig]] + 
    (.rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]) * .rebound_data[[I_E]]
  xend <- x_orig_energy - .rebound_data[[S_dot_dev]]
  yend <- y_orig_energy
  out <- add_segment(indexed = indexed,
                     colour = S_dot_dev_colour, size = S_dot_dev_size,
                     meta = meta, graph_type = energy_type, segment_name = S_dot_dev, 
                     x_orig = x_orig_energy, y_orig = y_orig_energy,
                     x = x_orig_energy, y = y_orig_energy, xend = xend, yend = yend)
  
  # Delta_E_dot_emb_star segment for energy graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_E_dot_emb_star]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_E_dot_emb_star_colour, size = Delta_E_dot_emb_star_size,
                meta = meta, graph_type = energy_type, segment_name = Delta_E_dot_emb_star, 
                x_orig = x_orig_energy, y_orig = y_orig_energy,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_md_star*I_E segment for energy graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_md_star]] * .rebound_data[[I_E]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_md_star_I_E_colour, size = Delta_C_dot_md_star_I_E_size,
                meta = meta, graph_type = energy_type, segment_name = paste0(Delta_C_dot_md_star, I_E), 
                x_orig = x_orig_energy, y_orig = y_orig_energy,
                x = x, y = y, xend = xend, yend = yend)
  
  # Substitution effect
  
  # Delta_E_dot_s_hat segment for energy graph (dsub)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_E_dot_s_hat]]
  yend <- y
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_E_dot_s_hat_colour, size = Delta_E_dot_s_hat_size,
                meta = meta, graph_type = energy_type, segment_name = Delta_E_dot_s_hat, 
                x_orig = x_orig_energy, y_orig = y_orig_energy,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_hat*I_E segment for energy graph (isub)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_hat]] * .rebound_data[[I_E]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_o_hat_I_E_colour, size = Delta_C_dot_o_hat_I_E_size,
                meta = meta, graph_type = energy_type, segment_name = paste0(Delta_C_dot_o_hat, I_E), 
                x_orig = x_orig_energy, y_orig = y_orig_energy,
                x = x, y = y, xend = xend, yend = yend)
  
  # Income effect
  
  # Delta_E_dot_s_bar segment for energy graph (dinc)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_E_dot_s_bar]]
  yend <- y
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_E_dot_s_bar_colour, size = Delta_E_dot_s_bar_size,
                meta = meta, graph_type = energy_type, segment_name = Delta_E_dot_s_bar, 
                x_orig = x_orig_energy, y_orig = y_orig_energy,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_bar*I_E segment for energy graph (iinc)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_bar]] * .rebound_data[[I_E]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_o_bar_I_E_colour, size = Delta_C_dot_o_bar_I_E_size,
                meta = meta, graph_type = energy_type, segment_name = paste0(Delta_C_dot_o_bar, I_E), 
                x_orig = x_orig_energy, y_orig = y_orig_energy,
                x = x, y = y, xend = xend, yend = yend)
  
  
  # Productivity effect (prod)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[k]] * .rebound_data[[N_dot_hat]] * .rebound_data[[I_E]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = prod_colour, size = prod_size,
                meta = meta, graph_type = energy_type, segment_name = "productivity", 
                x_orig = x_orig_energy, y_orig = y_orig_energy,
                x = x, y = y, xend = xend, yend = yend)
  
  
  
  #
  # Start of Cost graph
  # 
  
  # Emplacement effect
  
  # G_dot segment for cost graph
  x_orig_cost <- .rebound_data[[C_dot_s_orig]]
  y_orig_cost <- .rebound_data[[C_dot_cap_orig]] + .rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]
  xend <- x_orig_cost - .rebound_data[[G_dot]]
  yend <- y_orig_cost
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = G_dot_colour, size = G_dot_size,
                meta = meta, graph_type = cost_type, segment_name = G_dot, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x_orig_cost, y = y_orig_cost, xend = xend, yend = yend)
  
  # Delta_C_dot_cap_star segment for cost graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_cap_star]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_cap_star_colour, size = Delta_C_dot_cap_star_size,
                meta = meta, graph_type = cost_type, segment_name = Delta_C_dot_cap_star, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_md segment for cost graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_md_star]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_md_star_colour, size = Delta_C_dot_md_star_size,
                meta = meta, graph_type = cost_type, segment_name = Delta_C_dot_md_star, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Substitution effect
  
  # Delta_C_dot_s_hat segment for cost graph (dsub)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_C_dot_s_hat]]
  yend <- y
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_s_hat_colour, size = Delta_C_dot_s_hat_size,
                meta = meta, graph_type = cost_type, segment_name = Delta_C_dot_s_hat, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_hat segment for cost graph (isub)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_hat]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_o_hat_colour, size = Delta_C_dot_o_hat_size,
                meta = meta, graph_type = cost_type, segment_name = Delta_C_dot_o_hat, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Income effect
  
  # Delta_C_dot_s_bar segment for cost graph (dinc)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_C_dot_s_bar]]
  yend <- y
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_s_bar_colour, size = Delta_C_dot_s_bar_size,
                meta = meta, graph_type = cost_type, segment_name = Delta_C_dot_s_bar, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_bar segment for cost graph (iinc)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_bar]]
  out <- out %>% 
    add_segment(indexed = indexed,
                colour = Delta_C_dot_o_bar_colour, size = Delta_C_dot_o_bar_size,
                meta = meta, graph_type = cost_type, segment_name = Delta_C_dot_o_bar, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
}


#' Add a line segment to a data frame
#'
#' @param .DF 
#' @param indexed 
#' @param meta 
#' @param graph_type 
#' @param segment_name 
#' @param colour 
#' @param size 
#' @param x_orig 
#' @param y_orig 
#' @param x 
#' @param y 
#' @param xend 
#' @param yend 
#' @param arrow
#'
#' @return A version of `.DF` with line segments added as the final row.
#' 
#' @export
#'
#' @examples
add_segment <- function(.DF = NULL, indexed, meta, graph_type, segment_name, 
                        colour = "black", size = 1, linetype = "solid",
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
      size = size,
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






