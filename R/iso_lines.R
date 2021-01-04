#' Iso-energy lines for energy rebound paths
#' 
#' This function creates a data frame of constant-energy lines 
#' at various rebound stages.
#' The iso lines are described by slope and intercept.
#'
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
#' @param I_E,k See `ReboundTools::eeu_base_params`.
#' @param E_dot_s_orig,E_dot_emb_orig,C_dot_md_orig,C_dot_o_orig See `ReboundTools::orig_vars`.
#' @param S_dot_dev,E_dot_s_star,E_dot_emb_star,C_dot_md_star,C_dot_o_star See `ReboundTools::star_vars`.
#' @param E_dot_s_hat,E_dot_emb_hat,C_dot_md_hat,C_dot_o_hat,N_dot_hat See `ReboundTools::hat_vars`.
#' @param E_dot_s_bar,E_dot_emb_bar,C_dot_md_bar,C_dot_o_bar See `ReboundTools::bar_vars`.
#' @param E_dot_s_bar,E_dot_emb_bar,C_dot_md_bar,C_dot_o_bar See `ReboundTools::bar_vars`.
#' @param energy_type See `ReboundTools::graph_types`.
#' @param grid_colour See `ReboundTools::graph_colours`.
#' @param grid_size Line width for iso-energy lines. Default is `0.5`.
#' @param grid_linetype Line type for iso-energy lines. Default is "solid".
#'
#' @return A data frame containing iso-energy lines to be drawn on an energy rebound path graph.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   iso_energy_lines()
iso_energy_lines <- function(.rebound_data, 
                             indexed = FALSE, 
                             
                             I_E = ReboundTools::eeu_base_params$I_E, 
                             k = ReboundTools::eeu_base_params$k,
                             
                             E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig, 
                             E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig, 
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
                             N_dot_hat = ReboundTools::hat_vars$N_dot_hat, 
                             
                             E_dot_s_bar = ReboundTools::bar_vars$E_dot_s_bar, 
                             E_dot_emb_bar = ReboundTools::bar_vars$E_dot_emb_bar, 
                             C_dot_md_bar = ReboundTools::bar_vars$C_dot_md_bar,
                             C_dot_o_bar = ReboundTools::bar_vars$C_dot_o_bar,

                             E_dot_s_tilde = ReboundTools::tilde_vars$E_dot_s_tilde,
                             E_dot_emb_tilde = ReboundTools::tilde_vars$E_dot_emb_tilde,
                             C_dot_md_tilde = ReboundTools::tilde_vars$C_dot_md_tilde,
                             C_dot_o_tilde = ReboundTools::tilde_vars$C_dot_o_tilde,

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
                  iso_name = "Re = 100%", 
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
            iso_name = "Re = 0%", 
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
            iso_name = ReboundTools::rebound_terms$Re_empl, 
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
            iso_name = ReboundTools::rebound_terms$Re_sub, 
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
            iso_name = ReboundTools::rebound_terms$Re_inc, 
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
            iso_name = ReboundTools::rebound_terms$Re_tot,
            colour = grid_colour,
            size = grid_size,
            linetype = grid_linetype,
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  return(isos)
}



#' Create a data frame of iso-cost lines
#' 
#' This function creates a data frame of constant-cost lines 
#' at various rebound stages.
#' The iso lines are described by slope and intercept.
#' 
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
#' @param C_dot_s_orig,C_dot_cap_orig,C_dot_md_orig,C_dot_o_orig See `ReboundTools::orig_vars`.
#' @param G_dot See `ReboundTools::star_vars`.
#' @param cost_type See `ReboundTools::graph_types$cost`.
#' @param grid_colour See `ReboundTools::graph_colours`.
#' @param grid_size Line width. Default is `0.5`.
#' @param grid_linetype Line type. Default is "solid".
#'
#' @return A data frame of iso-cost lines for a cost graph.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   iso_cost_lines()
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
                  graph_type = cost_type, iso_name = ReboundTools::rebound_stages$orig,
                  colour = grid_colour, size = grid_size, linetype = grid_linetype, 
                  x_orig = x_orig, y_orig = y_orig, 
                  x = x, y = y)
  
  # Iso-cost line after expected savings
  x <- x - .rebound_data[[G_dot]]
  y <- y
  isos <- isos %>% 
    add_iso(indexed = indexed, meta = meta, 
            graph_type = cost_type, iso_name = "G_dot",
            colour = grid_colour, size = grid_size, linetype = grid_linetype, 
            x_orig = x_orig, y_orig = y_orig, 
            x = x, y = y)

  return(isos)
}


#' Create iso-budget (expenditure) lines for a preferences graph
#' 
#' This function creates a data frame of constant-budget (expenditure) lines 
#' at various rebound stages.
#' The iso lines are described by slope and intercept.
#' 
#' The preferences graph is _always_ indexed, so there is no `indexed` argument.
#'
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#' @param p_s_orig,q_dot_s_orig,C_dot_cap_orig,C_dot_md_orig,C_dot_o_orig,M_dot_orig See `ReboundTools::orig_vars`.
#' @param p_s_star,G_dot See `ReboundTools::star_vars`.
#' @param q_dot_s_hat.C_dot_o_hat See `ReboundTools::hat_vars`.
#' @param Delta_q_dot_s_hat,Delta_C_dot_cap_star,Delta_C_dot_md_star,Delta_C_dot_o_hat See `ReboundTools::Delta_vars`.
#' @param prefs_type See `ReboundTools::graph_types`.
#' @param grid_colour See `ReboundTools::graph_colours`.
#' @param grid_size Line width. Default is `0.2`.
#' @param grid_linetype Line type. Default is "solid".
#'
#' @return A data frame of iso-budget lines for a preferences graph.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   iso_budget_lines_prefs()
iso_budget_lines_prefs <- function(.rebound_data, 
                                   
                                   p_s_orig = ReboundTools::orig_vars$p_s_orig, 
                                   q_dot_s_orig = ReboundTools::orig_vars$q_dot_s_orig,
                                   C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig,
                                   C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                                   C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                                   M_dot_orig = ReboundTools::orig_vars$M_dot_orig,
                                   
                                   p_s_star = ReboundTools::star_vars$p_s_star,
                                   G_dot = ReboundTools::star_vars$G_dot,
                                   
                                   q_dot_s_hat = ReboundTools::hat_vars$q_dot_s_hat,
                                   C_dot_o_hat = ReboundTools::hat_vars$C_dot_o_hat, 
                                   
                                   Delta_q_dot_s_hat = ReboundTools::Delta_vars$Delta_q_dot_s_hat,
                                   Delta_C_dot_cap_star = ReboundTools::Delta_vars$Delta_C_dot_cap_star,
                                   Delta_C_dot_md_star = ReboundTools::Delta_vars$Delta_C_dot_md_star,
                                   Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                                   
                                   prefs_type = ReboundTools::graph_types$preferences,
                                   grid_colour = ReboundTools::graph_colours$grid,
                                   grid_size = 0.2,
                                   grid_linetype = "solid") {
  
  meta <- extract_meta(.rebound_data)
  
  # Iso-budget line at the orig point.
  # slope_orig <- -p_s_orig * q_dot_s_orig / C_dot_o_orig
  slope_orig <- -.rebound_data[[p_s_orig]] * .rebound_data[[q_dot_s_orig]] / .rebound_data[[C_dot_o_orig]]
  # intercept_orig <- (M_dot_orig - C_dot_cap_orig - C_dot_md_orig) / C_dot_o_orig
  intercept_orig <- (.rebound_data[[M_dot_orig]] - .rebound_data[[C_dot_cap_orig]] - 
                       .rebound_data[[C_dot_md_orig]]) / 
    .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(meta = meta, 
                         graph_type = prefs_type,
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
                         graph_type = prefs_type,
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
                         graph_type = prefs_type,
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
                         graph_type = prefs_type,
                         line_name = ReboundTools::rebound_stages$bar, 
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
  out <- add_budget_line(out, meta = meta, 
          graph_type = prefs_type, 
          line_name = "ray",
          colour = grid_colour, size = grid_size, linetype = grid_linetype, 
          slope = slope_ray, 
          intercept = intercept_ray)
  return(out)
}


#' Indifference curve lines
#' 
#' This function calculates indifference curves 
#' to be drawn on the preferences graph.
#'
#' The preferences graph is _always_ indexed, so there is no `indexed` argument.
#'
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#' @param q_dot_s_orig,C_dot_o_orig,f_Cs_orig,sigma See `ReboundTools::orig_vars`.
#' @param q_dot_s_star,C_dot_o_star See `ReboundTools::star_vars`.
#' @param prefs_type See `ReboundTools::graph_types`.
#' @param grid_colour See `ReboundTools::graph_colours`.
#' @param grid_size Width of indifference curve lines. Default is `0.2`.
#' @param grid_linetype = Line type for indifference curve lines. Default is "solid".
#'
#' @return A data frame of indifference curves for a preferences graph.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   indifference_lines()
indifference_lines <- function(.rebound_data, 
                               q_dot_s_orig = ReboundTools::orig_vars$q_dot_s_orig,
                               C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                               f_Cs_orig = ReboundTools::orig_vars$f_Cs_orig,
                               sigma = ReboundTools::orig_vars$sigma,

                               q_dot_s_star = ReboundTools::star_vars$q_dot_s_star,
                               C_dot_o_star = ReboundTools::star_vars$C_dot_o_star,
                               
                               prefs_type = ReboundTools::graph_types$preferences,
                               grid_colour = ReboundTools::graph_colours$grid,
                               grid_size = 0.2,
                               grid_linetype = "solid") {
  
  meta <- extract_meta(.rebound_data)

  # Original data.  
  qs0 <- .rebound_data[[q_dot_s_orig]]
  Co0 <- .rebound_data[[C_dot_o_orig]]
  f_Cs0 <- .rebound_data[[f_Cs_orig]]
  sigma_val <- .rebound_data[[sigma]]
  
  # Indifference curve at the orig point
  qs1 <- qs0
  Co1 <- Co0
  qs1_qs0 <- qs1/qs0
  Co1_Co0 <- Co1/Co0
  icurves <- add_indifference_curve(meta = meta,
                                    graph_type = prefs_type,
                                    line_name = ReboundTools::rebound_stages$orig,
                                    colour = grid_colour,
                                    size = grid_size,
                                    linetype = grid_linetype,
                                    qs1_qs0 = qs1_qs0,
                                    Co1_Co0 = Co1_Co0,
                                    f_Cs_orig = f_Cs0,
                                    sigma = sigma_val)
  
  # Indifference curve at the star point (after emplacement, before substitution)
  # qs1 <- .rebound_data[[q_dot_s_star]]
  # Co1 <- .rebound_data[[C_dot_o_star]]
  # qs1_qs0 <- qs1/qs0
  # Co1_Co0 <- Co1/Co0
  # icurves <- icurves %>%
  #   add_indifference_curve(meta = meta,
  #                          graph_type = prefs_type,
  #                          line_name = ReboundTools::rebound_stages$orig,
  #                          colour = grid_colour,
  #                          size = grid_size,
  #                          linetype = grid_linetype,
  #                          qs1_qs0 = qs1_qs0,
  #                          Co1_Co0 = Co1_Co0,
  #                          f_Cs_orig = f_Cs0,
  #                          sigma = sigma_val)
  
  
  return(icurves)
}


#' Add a budget line to a data frame of budget lines
#' 
#' Adds a budget line to a data frame. 
#' The budget lines are accumulated in rows.
#' 
#' There is usually no need to call this function directly. 
#' Functions like `iso_budget_lines_prefs()` `add_iso()` 
#' call `add_budget_line()` internally.
#'
#' @param .DF The data frame to which lines are appended. 
#'            When `NULL`, the default, a new data frame is created and returned.
#'            When not `NULL`, rows for the budget lines are added to the bottom of `.DF`.
#' @param meta A data frame of metadata for the segment to be added. 
#'             This metadata data frame provides the left-most columns of the return value.
#' @param graph_type The type of graph associated with this segment. 
#'                   Default is See `ReboundTools::graph_types$preferences`.
#' @param line_name A name for this budget line 
#' @param colour The colour for this budget line. Default is `ReboundTools::graph_colours$grid`.
#' @param size The size (width) for this budget line Default is `0.5`.
#' @param linetype The line type for this budget line Default is "solid".
#' @param slope The slope of this budget line.
#' @param intercept The intercept of this budget line.
#'
#' @return A version of `.DF` with budget lines added at the bottom.
#' 
#' @export
#'
#' @examples
#' meta <- tibble::tibble(Case = "Test case")
#' add_budget_line(meta = meta, 
#'   line_name = "Test budget line", 
#'   slope = -1, intercept = 5)
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
#' Adds a iso line to a data frame. 
#' The iso lines are accumulated in rows at the bottom of the data frame.
#' 
#' There is usually no need to call this function directly. 
#' Functions like `iso_energy_lines()` and `iso_cost_lines()` 
#' call `add_iso()` internally.
#'
#' @param .DF A data frame that accumulates iso lines. 
#'            When `NULL`, the default, a new data frame is created and returned.
#'            When not `NULL`, rows for the segment are added to the bottom of `.DF`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
#' @param meta A data frame of metadata for the segment to be added. 
#'             This metadata data frame provides the left-most columns of the return value.
#' @param graph_type The type of graph associated with this segment. See `ReboundTools::graph_types`.
#' @param iso_name A name for this iso line.
#' @param colour The colour for this iso line. Default is `ReboundTools::graph_colours$grid`.
#' @param size Line width. Default is `0.5`.
#' @param linetype Line type for this iso line. Default is "solid". 
#' @param x_orig,y_orig The (x,y) coordinates of the starting point for the path on this graph, 
#'                      used for indexing.
#' @param x,y The (x,y) coordinates of a point on this iso line. 
#'            Slope and intercept are calculated from these values.
#'
#' @return A version of `.DF` with iso lines added at the bottom.
#' 
#' @export
#'
#' @examples
#' meta <- tibble::tibble(Case = "Test case")
#' add_iso(meta = meta, graph_type = "Test type", 
#'   iso_name = "Test segment", 
#'   x_orig = 10, y_orig = 10, 
#'   x = 20, y = 30)
add_iso <- function(.DF = NULL, indexed = FALSE, meta, graph_type, iso_name, 
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
                  line_name = iso_name, colour = colour, 
                  size = size, linetype = linetype, 
                  slope = slope, intercept = intercept)
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
#' @param qs1_qs0 
#' @param Co1_Co0 
#' @param f_Cs_orig 
#' @param sigma 
#'
#' @return
#' @export
#'
#' @examples
add_indifference_curve <- function(.DF = NULL, meta, graph_type = ReboundTools::graph_types$preferences, 
                                   line_name, 
                                   colour = ReboundTools::graph_colours$grid, size = 0.5, linetype = "solid",
                                   qs1_qs0, Co1_Co0, f_Cs_orig, sigma) {
  out <- meta %>% 
    dplyr::mutate(
      graph_type = graph_type, 
      line_name = line_name, 
      colour = colour,
      size = size,
      linetype = linetype,
      qs1_qs0 = qs1_qs0,
      Co1_Co0 = Co1_Co0,
      f_Cs_orig = f_Cs_orig, 
      sigma = sigma
    )
  if (is.null(.DF)) {
    return(out)
  }
  .DF %>% 
    dplyr::bind_rows(out)
}


#' An indifference curve
#' 
#' This function gives points along an indifference curve in 
#' (q_dot_s/q_dot_s_orig, C_dot_o/C_dot_o_orig) space.
#' The indifference curve assumes CES utility. 
#' The equation of the indifference curve is
#' `u_dot/u_dot_orig = [f_Cs*(q_dot_s/q_dot_s_orig)^rho + (1-f_Cs)*(C_dot_o/C_dot_o_orig)^rho]^(1/rho)`.
#' 
#' This function is vectorized.
#'
#' @param qs_qs0 The ratio `q_dot_s/q_dot_s_orig`. Sweeping this variable (x) gives
#'               the indifference curve parameterized by the other arguments.
#' @param qs1_qs0 The x coordinate of a point on this indifference curve.
#' @param Co1_Co0 The y coordinate of a point on this indifference curve.
#' @param f_Cs_orig The fraction of original spending on the energy service relative to the sum of energy service and other goods spending, calculated by `C_dot_s_orig / (C_dot_s_orig + C_dot_o_orig)`.
#' @param sigma The elasticity of substitution between the energy service and other goods.
#' @param rho The exponent in the CES utility function. Default is `(sigma-1)/sigma`.
#'
#' @return The value of `C_dot_o/C_dot_o_orig`, given values of remaining arguments.
#' 
#' @export
#'
#' @examples
#' qs1_qs0 <- 1
#' Co1_Co0 <- 1
#' sigma <- 0.3
#' f_Cs_orig <- 0.01
#' DF <- data.frame(x = seq(0.5, 1.5, by = 0.1)) %>% 
#'   dplyr::mutate(
#'     y = indifference_func(x, qs1_qs0 = qs1_qs0, Co1_Co0 = Co1_Co0, 
#'                           f_Cs_orig = f_Cs_orig, sigma = sigma)
#'   )
#' DF
indifference_func <- function(qs_qs0, qs1_qs0, Co1_Co0, f_Cs_orig, sigma, rho = (sigma-1)/sigma) {
  term1 <- f_Cs_orig/(1 - f_Cs_orig)
  term2 <- qs1_qs0^rho
  term3 <- qs_qs0^rho
  term4 <- Co1_Co0^rho
  (term1*(term2 - term3) + term4)^(1/rho)
}



