
#' Create a data frame of iso-cost lines
#' 
#' This function creates a data frame of constant-energy or constant-cost lines 
#' at various rebound stages.
#' The iso lines are described by slope and intercept.
#' 
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#'
#' @return
#' 
#' @export
#'
#' @examples
iso_cost_lines <- function(.rebound_data, 
                           indexed = FALSE,
                           C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig, 
                           C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig, 
                           C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig, 
                           C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig, 
                           
                           G_dot = ReboundTools::star_vars$G_dot,
                           
                           cost_type = ReboundTools::graph_types$cost,
                           grid_colour = ReboundTools::graph_colours$grid,
                           grid_size = 0.5,
                           grid_linetype = "solid") {
  
  meta <- extract_meta(.rebound_data)
  
  # Iso-cost line at the orig point.
  x_orig <- .rebound_data[[C_dot_s_orig]]
  y_orig <- .rebound_data[[C_dot_cap_orig]] + .rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]
  x <- x_orig
  y <- y_orig
  isos <- add_iso(indexed = indexed, meta = meta, 
                  graph_type = cost_type, line_name = "orig",
                  colour = grid_colour, size = grid_size, linetype = grid_linetype, 
                  x_orig = x_orig, y_orig = y_orig, 
                  x = x, y = y)
  
  # Iso-cost line after expected savings
  x <- x - .rebound_data[[G_dot]]
  y <- y
  isos <- isos %>% 
    add_iso(indexed = indexed, meta = meta, 
            graph_type = cost_type, line_name = "G_dot",
            colour = grid_colour, size = grid_size, linetype = grid_linetype, 
            x_orig = x_orig, y_orig = y_orig, 
            x = x, y = y)

  return(isos)
}


#' Title
#'
#' @param .rebound_data 
#'
#' @return
#' @export
#'
#' @examples
iso_budget_lines_prefs <- function(.rebound_data, 
                                   
                                   p_s_orig = ReboundTools::orig_vars$p_s_orig, 
                                   q_dot_s_orig = ReboundTools::orig_vars$q_dot_s_orig,
                                   C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                                   M_dot_orig = ReboundTools::orig_vars$M_dot_orig,
                                   C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig,
                                   C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                                   orig_size = 0.5,
                                   
                                   p_s_star = ReboundTools::star_vars$p_s_star,
                                   G_dot = ReboundTools::star_vars$G_dot,
                                   Delta_q_dot_s_hat = ReboundTools::Delta_vars$Delta_q_dot_s_hat,
                                   Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                                   
                                   Delta_C_dot_cap_star = ReboundTools::Delta_vars$Delta_C_dot_cap_star,
                                   Delta_C_dot_md_star = ReboundTools::Delta_vars$Delta_C_dot_md_star,
                                     
                                   grid_colour = ReboundTools::graph_colours$grid) {
  
  meta <- extract_meta(.rebound_data)
  
  # Iso-budget line at the orig point.
  # slope_orig <- -p_s_orig * q_dot_s_orig / C_dot_o_orig
  slope_orig <- -.rebound_data[[p_s_orig]] * .rebound_data[[q_dot_s_orig]] / .rebound_data[[C_dot_o_orig]]
  # intercept_orig <- (M_dot_orig - C_dot_cap_orig - C_dot_md_orig) / C_dot_o_orig
  intercept_orig <- (.rebound_data[[M_dot_orig]] - .rebound_data[[C_dot_cap_orig]] - 
                       .rebound_data[[C_dot_md_orig]]) / 
    .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(meta = meta, 
                         graph_type = ReboundTools::graph_types$preferences,
                         line_name = ReboundTools::rebound_stages$orig, 
                         colour = grid_colour, 
                         size = orig_size, 
                         linetype = "solid", 
                         slope = slope_orig,
                         intercept = intercept_orig)
  
  # Iso-budget line at the star point (after emplacement, before substitution)
  # slope_star <- -p_s_star * q_dot_s_orig / C_dot_o_orig
  slope_star <- -.rebound_data[[p_s_star]] * .rebound_data[[q_dot_s_orig]] / .rebound_data[[C_dot_o_orig]]
  # intercept_star <- (M_dot_orig - C_dot_cap_orig - C_dot_md_orig - G_dot) / C_dot_o_orig
  intercept_star <- (.rebound_data[[M_dot_orig]] - .rebound_data[[C_dot_cap_orig]] - 
                       .rebound_data[[C_dot_md_orig]] - .rebound_data[[G_dot]]) / 
    .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(out, meta = meta, 
                         graph_type = ReboundTools::graph_types$preferences,
                         line_name = ReboundTools::rebound_stages$star, 
                         colour = grid_colour, 
                         size = orig_size, 
                         linetype = "solid", 
                         slope = slope_star,
                         intercept = intercept_star)
  
  # After this point, the slope doesn't change, but the intercept does.
  
  # Iso-budget line at the hat point (after substitution, before income)
  slope_hat <- slope_star
  # intercept_hat <- (M_dot_orig - C_dot_cap_orig - C_dot_md_orig - G_dot + p_s_star*Delta_q_dot_s_hat + Delta_C_dot_o_hat) / C_dot_o_orig
  intercept_hat <- (.rebound_data[[M_dot_orig]] - .rebound_data[[C_dot_cap_orig]] - 
                      .rebound_data[[C_dot_md_orig]] - .rebound_data[[G_dot]] +
                      .rebound_data[[p_s_star]] * .rebound_data[[Delta_q_dot_s_hat]] + 
                      .rebound_data[[Delta_C_dot_o_hat]]) / 
    .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(out, meta = meta, 
                         graph_type = ReboundTools::graph_types$preferences,
                         line_name = ReboundTools::rebound_stages$hat, 
                         colour = grid_colour, 
                         size = orig_size, 
                         linetype = "solid", 
                         slope = slope_hat,
                         intercept = intercept_hat)
  
  # Iso-budget line at the bar point (after income, before productivity)
  slope_bar <- slope_hat
  # intercept_bar <- (M_dot_orig - C_dot_cap_orig - C_dot_md_orig - Delta_C_dot_cap_star - Delta_C_dot_md_star) / C_dot_o_orig
  intercept_bar <- (.rebound_data[[M_dot_orig]] - .rebound_data[[C_dot_cap_orig]] - 
                      .rebound_data[[C_dot_md_orig]] - .rebound_data[[Delta_C_dot_cap_star]] - 
                      .rebound_data[[Delta_C_dot_md_star]]) / 
    .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(out, meta = meta, 
                         graph_type = ReboundTools::graph_types$preferences,
                         line_name = ReboundTools::rebound_stages$hat, 
                         colour = grid_colour, 
                         size = orig_size, 
                         linetype = "solid", 
                         slope = slope_bar,
                         intercept = intercept_bar)
  
  return(out)
  
}



#' Title
#'
#' @param .DF 
#' @param meta 
#' @param graph_type 
#' @param line_name 
#' @param colour 
#' @param size 
#' @param linetype 
#' @param slope 
#' @param intercept 
#'
#' @return
#' @export
#'
#' @examples
add_budget_line <- function(.DF = NULL, meta, graph_type = ReboundTools::graph_types$preferences, 
                                  line_name,
                                  colour = ReboundTools::graph_colours$grid, size = 0.5, linetype = "solid",
                                  slope, intercept) {
  out <- meta %>% 
    dplyr::mutate(
      graph_type = graph_type, 
      line_name = line_name, 
      colour = colour,
      size = size,
      linetype = linetype,
      slope = slope,
      intercept = intercept
    )
  if (is.null(.DF)) {
    return(out)
  }
  .DF %>% 
    dplyr::bind_rows(out)
}


#' Add an iso line description to a data frame
#'
#' @param .DF 
#' @param indexed 
#' @param meta 
#' @param graph_type 
#' @param iso_name 
#' @param colour 
#' @param size 
#' @param linetype 
#' @param x_orig 
#' @param y_orig 
#' @param x 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
add_iso <- function(.DF = NULL, indexed = FALSE, meta, graph_type, line_name, 
        colour = ReboundTools::graph_colours$grid, size = 0.5, linetype = "solid", 
        x_orig, y_orig, 
        x, y) {
  if (indexed) {
    slope <- -x_orig/y_orig
    intercept <- (x + y)/y_orig
  } else {
    slope <- -1
    intercept <- x + y
  }
  add_budget_line(.DF, meta = meta, graph_type = graph_type,
                  line_name = line_name, colour = colour, 
                  size = size, linetype = linetype, 
                  slope = slope, intercept = intercept)
}


