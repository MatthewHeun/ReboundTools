#' Title
#'
#' @param .rebound_data 
#' @param indexed 
#'
#' @return
#' @export
#'
#' @examples
iso_energy_lines <- function(.rebound_data, 
                             indexed = FALSE, 
                             E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig, 
                             E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig, 
                             I_E = ReboundTools::eeu_base_params$I_E, 
                             C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                             C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                             S_dot_dev = ReboundTools::star_vars$S_dot_dev,
                             
                             E_dot_s_star = ReboundTools::star_vars$E_dot_s_star, 
                             E_dot_emb_star = ReboundTools::star_vars$E_dot_emb_star, 
                             C_dot_md_star = ReboundTools::star_vars$C_dot_md_star,
                             C_dot_o_star = ReboundTools::star_vars$C_dot_o_star,

                             E_dot_s_hat = ReboundTools::hat_vars$E_dot_s_hat, 
                             E_dot_emb_hat = ReboundTools::hat_vars$E_dot_emb_hat, 
                             C_dot_md_hat = ReboundTools::hat_vars$C_dot_md_hat,
                             C_dot_o_hat = ReboundTools::hat_vars$C_dot_o_hat,
                             
                             E_dot_s_bar = ReboundTools::bar_vars$E_dot_s_bar, 
                             E_dot_emb_bar = ReboundTools::bar_vars$E_dot_emb_bar, 
                             C_dot_md_bar = ReboundTools::bar_vars$C_dot_md_bar,
                             C_dot_o_bar = ReboundTools::bar_vars$C_dot_o_bar,

                             E_dot_s_tilde = ReboundTools::tilde_vars$E_dot_s_tilde,
                             E_dot_emb_tilde = ReboundTools::tilde_vars$E_dot_emb_tilde,
                             C_dot_md_tilde = ReboundTools::tilde_vars$C_dot_md_tilde,
                             C_dot_o_tilde = ReboundTools::tilde_vars$C_dot_o_tilde,
                             k = ReboundTools::eeu_base_params$k,
                             N_dot_hat = ReboundTools::hat_vars$N_dot_hat, 

                             energy_type = ReboundTools::graph_types$energy,
                             grid_colour = ReboundTools::graph_colours$grid,
                             grid_size = 0.5,
                             grid_linetype = "solid") {
  
  meta <- extract_meta(.rebound_data)
  
  # Iso-energy line at the orig point.
  # This is the 100% Rebound line.
  x_orig <- .rebound_data[[E_dot_s_orig]]
  y_orig <- .rebound_data[[E_dot_emb_orig]] + 
    (.rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]) * .rebound_data[[I_E]]
  
  x <- x_orig
  y <- y_orig
  isos <- add_iso(indexed = indexed, 
                  meta = meta, 
                  graph_type = energy_type, 
                  line_name = "Re = 100%", 
                  colour = grid_colour, 
                  size = grid_size, 
                  linetype = grid_linetype, 
                  x_orig = x_orig, y_orig = y_orig,
                  x = x, y = y)
  
  # Iso-energy line at the end of the S_dot_dev line.  
  # This is the 0% rebound line.
  x <- x_orig - .rebound_data[[S_dot_dev]]
  y <- y
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = energy_type, 
            line_name = "Re = 0%", 
            colour = grid_colour, 
            size = grid_size, 
            linetype = grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  # Iso-energy line at the end of the emplacement effect.
  # This gives Re_empl.
  x <- .rebound_data[[E_dot_s_star]]
  y <- .rebound_data[[E_dot_emb_star]] + 
    (.rebound_data[[C_dot_md_star]] + .rebound_data[[C_dot_o_star]]) * .rebound_data[[I_E]]
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = energy_type, 
            line_name = ReboundTools::rebound_terms$Re_empl, 
            colour = grid_colour, 
            size = grid_size, 
            linetype = grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  # Iso-energy line at the end of the substitution effect.
  # This gives Re_sub.
  x <- .rebound_data[[E_dot_s_hat]]
  y <- .rebound_data[[E_dot_emb_hat]] + 
    (.rebound_data[[C_dot_md_hat]] + .rebound_data[[C_dot_o_hat]]) * .rebound_data[[I_E]]
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = energy_type, 
            line_name = ReboundTools::rebound_terms$Re_sub, 
            colour = grid_colour, 
            size = grid_size, 
            linetype = grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  # Iso-energy line at the end of the income effect.
  # This gives Re_inc.
  x <- .rebound_data[[E_dot_s_bar]]
  y <- .rebound_data[[E_dot_emb_bar]] + 
    (.rebound_data[[C_dot_md_bar]] + .rebound_data[[C_dot_o_bar]]) * .rebound_data[[I_E]]
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = energy_type, 
            line_name = ReboundTools::rebound_terms$Re_inc, 
            colour = grid_colour, 
            size = grid_size, 
            linetype = grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  # Iso-energy line at the end of the path.
  # This is the Re_tot line.
  x <- .rebound_data[[E_dot_s_tilde]]
  y <- .rebound_data[[E_dot_emb_tilde]] + 
    (.rebound_data[[C_dot_md_tilde]] + .rebound_data[[C_dot_o_tilde]]) * .rebound_data[[I_E]] + 
    .rebound_data[[k]] * .rebound_data[[N_dot_hat]] * .rebound_data[[I_E]]
  isos <- isos %>%
    add_iso(indexed = indexed,
            meta = meta,
            graph_type = energy_type,
            line_name = ReboundTools::rebound_terms$Re_tot,
            colour = grid_colour,
            size = grid_size,
            linetype = grid_linetype,
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  return(isos)
}



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
                  graph_type = cost_type, line_name = ReboundTools::rebound_stages$orig,
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
                                   
                                   p_s_star = ReboundTools::star_vars$p_s_star,
                                   G_dot = ReboundTools::star_vars$G_dot,
                                   Delta_q_dot_s_hat = ReboundTools::Delta_vars$Delta_q_dot_s_hat,
                                   Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                                   
                                   Delta_C_dot_cap_star = ReboundTools::Delta_vars$Delta_C_dot_cap_star,
                                   Delta_C_dot_md_star = ReboundTools::Delta_vars$Delta_C_dot_md_star,
                                   
                                   C_dot_o_hat = ReboundTools::hat_vars$C_dot_o_hat, 
                                   q_dot_s_hat = ReboundTools::hat_vars$q_dot_s_hat,
                                     
                                   grid_colour = ReboundTools::graph_colours$grid,
                                   grid_linetype = "solid",
                                   grid_size = 0.2) {
  
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
                         size = grid_size, 
                         linetype = grid_linetype, 
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
                         size = grid_size, 
                         linetype = grid_linetype, 
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
                         size = grid_size, 
                         linetype = grid_linetype, 
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
                         size = grid_size, 
                         linetype = grid_linetype, 
                         slope = slope_bar,
                         intercept = intercept_bar)

  # Add ray from origin through income points.
  # slope_ray <- (C_dot_o_hat / C_dot_o_orig) / (q_dot_s_hat / q_dot_s_orig)
  slope_ray <- (.rebound_data[[C_dot_o_hat]] / .rebound_data[[C_dot_o_orig]]) /
                  (.rebound_data[[q_dot_s_hat]] / .rebound_data[[q_dot_s_orig]])
  intercept_ray <- 0
  
  # Set indexed to FALSE, because we have already indexed the ray by adjusting the slope.
  out <- add_budget_line(out, meta = meta, 
          graph_type = ReboundTools::graph_types$preferences, 
          line_name = "ray",
          colour = grid_colour, size = grid_size, linetype = grid_linetype, 
          slope = slope_ray, 
          intercept = intercept_ray)
  
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
  if (!indexed) {
    # In regular (non-indexed) space, 
    # the slope of the iso line going through point x1, y1 is 
    # y = -x + (x1 + y1)
    # Thus, the slope is -1, and the intercept is x1 + y1.
    slope <- -1
    intercept <- x + y
  } else {
    # In the indexed version of the graph, 
    # the x and y axes are indexed by their orig values.
    # The derivation of the equation of the line in indexed space is as follows:
    # Start with the original iso line that goes through points x1 and y1:
    # y = -x + (x1 + y1)
    # Divide and multiply y by y_orig. Divide and multiply x by x_orig.
    # (y/o_orig) * y_orig = -(x/x_orig) * x_orig + (x1 + y1)
    # At this point, we have the indexed versions of x and y, but
    # we need to get y/y_orig alone on the left side of the equation.
    # Divide both sides by y_orig
    # (y/y_orig) = -(x/x_orig)*x_orig / y_orig + (x1 + y1)/y_orig
    # Rearrange
    # (y/y_orig) = (-x_orig/y_orig)*(x/x_orig) + (x1 + y1)/y_orig
    # Thus, the slope of the iso line in indexed space is (-x_orig/y_orig), and
    # the intercept of the iso line in indexed space is (x1 + y1)/y_orig.
    slope <- -x_orig/y_orig
    intercept <- (x + y)/y_orig
  }
  add_budget_line(.DF, meta = meta, graph_type = graph_type,
                  line_name = line_name, colour = colour, 
                  size = size, linetype = linetype, 
                  slope = slope, intercept = intercept)
}


