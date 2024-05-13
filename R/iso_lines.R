#' Iso-energy lines for energy rebound path graphs
#' 
#' This function creates a data frame of constant-energy lines 
#' at various rebound stages.
#' The iso lines are described by slope and intercept.
#'
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param graph_params See `ReboundTools::path_graph_params`.
#' @param I_E,k See `ReboundTools::eeu_base_params`.
#' @param E_dot_s_orig,E_dot_emb_orig,C_dot_omd_orig,C_dot_o_orig See `ReboundTools::orig_vars`.
#' @param S_dot_dev,E_dot_s_star,E_dot_emb_star,C_dot_omd_star,C_dot_o_star See `ReboundTools::star_vars`.
#' @param E_dot_s_hat,E_dot_emb_hat,C_dot_omd_hat,C_dot_o_hat,N_dot_hat See `ReboundTools::hat_vars`.
#' @param E_dot_s_bar,E_dot_emb_bar,C_dot_omd_bar,C_dot_o_bar See `ReboundTools::bar_vars`.
#' @param E_dot_s_bar,E_dot_emb_bar,C_dot_omd_bar,C_dot_o_bar See `ReboundTools::bar_vars`.
#' @param E_dot_s_tilde,E_dot_emb_tilde,C_dot_omd_tilde,C_dot_o_tilde See `ReboundTools::tilde_vars`.
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
                             graph_type = ReboundTools::graph_types$energy,
                             graph_params = ReboundTools::path_graph_params,
                             
                             I_E = ReboundTools::eeu_base_params$I_E, 
                             k = ReboundTools::eeu_base_params$k,
                             
                             E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig, 
                             E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig, 
                             C_dot_omd_orig = ReboundTools::orig_vars$C_dot_omd_orig,
                             C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                             
                             S_dot_dev = ReboundTools::star_vars$S_dot_dev,
                             E_dot_s_star = ReboundTools::star_vars$E_dot_s_star, 
                             E_dot_emb_star = ReboundTools::star_vars$E_dot_emb_star, 
                             C_dot_omd_star = ReboundTools::star_vars$C_dot_omd_star,
                             C_dot_o_star = ReboundTools::star_vars$C_dot_o_star,

                             E_dot_s_hat = ReboundTools::hat_vars$E_dot_s_hat, 
                             E_dot_emb_hat = ReboundTools::hat_vars$E_dot_emb_hat, 
                             C_dot_omd_hat = ReboundTools::hat_vars$C_dot_omd_hat,
                             C_dot_o_hat = ReboundTools::hat_vars$C_dot_o_hat,
                             N_dot_hat = ReboundTools::hat_vars$N_dot_hat, 
                             
                             E_dot_s_bar = ReboundTools::bar_vars$E_dot_s_bar, 
                             E_dot_emb_bar = ReboundTools::bar_vars$E_dot_emb_bar, 
                             C_dot_omd_bar = ReboundTools::bar_vars$C_dot_omd_bar,
                             C_dot_o_bar = ReboundTools::bar_vars$C_dot_o_bar,

                             E_dot_s_tilde = ReboundTools::tilde_vars$E_dot_s_tilde,
                             E_dot_emb_tilde = ReboundTools::tilde_vars$E_dot_emb_tilde,
                             C_dot_omd_tilde = ReboundTools::tilde_vars$C_dot_omd_tilde,
                             C_dot_o_tilde = ReboundTools::tilde_vars$C_dot_o_tilde) {
  
  meta <- extract_meta(.rebound_data)
  
  # Iso-energy line at the orig point.
  # This is the 100% Rebound line.
  x_orig <- .rebound_data[[E_dot_s_orig]]
  y_orig <- .rebound_data[[E_dot_emb_orig]] + 
    (.rebound_data[[C_dot_omd_orig]] + .rebound_data[[C_dot_o_orig]]) * .rebound_data[[I_E]]
  
  x <- x_orig
  y <- y_orig
  isos <- add_iso(indexed = indexed, 
                  meta = meta, 
                  graph_type = graph_type, 
                  iso_name = "Re = 100%", 
                  colour = graph_params$hundred_perc_rebound_grid_colour, 
                  linewidth = graph_params$hundred_perc_rebound_grid_linewidth, 
                  linetype = graph_params$hundred_perc_rebound_grid_linetype, 
                  x_orig = x_orig, y_orig = y_orig,
                  x = x, y = y)
  
  # Iso-energy line at the end of the S_dot_dev line.  
  # This is the 0% rebound line.
  x <- x_orig - .rebound_data[[S_dot_dev]]
  y <- y
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = graph_type, 
            iso_name = "Re = 0%", 
            colour = graph_params$zero_perc_rebound_grid_colour, 
            linewidth = graph_params$zero_perc_rebound_grid_linewidth, 
            linetype = graph_params$zero_perc_rebound_grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  # Iso-energy line at the end of the emplacement effect.
  # This gives Re_empl.
  x <- .rebound_data[[E_dot_s_star]]
  y <- .rebound_data[[E_dot_emb_star]] + 
    (.rebound_data[[C_dot_omd_star]] + .rebound_data[[C_dot_o_star]]) * .rebound_data[[I_E]]
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = graph_type, 
            iso_name = ReboundTools::rebound_terms$Re_empl, 
            colour = graph_params$energy_grid_colour, 
            linewidth = graph_params$energy_grid_linewidth, 
            linetype = graph_params$energy_grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  # Iso-energy line at the end of the substitution effect.
  # This gives Re_sub.
  x <- .rebound_data[[E_dot_s_hat]]
  y <- .rebound_data[[E_dot_emb_hat]] + 
    (.rebound_data[[C_dot_omd_hat]] + .rebound_data[[C_dot_o_hat]]) * .rebound_data[[I_E]]
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = graph_type, 
            iso_name = ReboundTools::rebound_terms$Re_sub, 
            colour = graph_params$energy_grid_colour, 
            linewidth = graph_params$energy_grid_linewidth, 
            linetype = graph_params$energy_grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  # Iso-energy line at the end of the income effect.
  # This gives Re_inc.
  x <- .rebound_data[[E_dot_s_bar]]
  y <- .rebound_data[[E_dot_emb_bar]] + 
    (.rebound_data[[C_dot_omd_bar]] + .rebound_data[[C_dot_o_bar]]) * .rebound_data[[I_E]]
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = graph_type, 
            iso_name = ReboundTools::rebound_terms$Re_inc, 
            colour = graph_params$energy_grid_colour, 
            linewidth = graph_params$energy_grid_linewidth, 
            linetype = graph_params$energy_grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  # Iso-energy line at the end of the path.
  # This is the Re_tot line.
  x <- .rebound_data[[E_dot_s_tilde]]
  y <- .rebound_data[[E_dot_emb_tilde]] + 
    (.rebound_data[[C_dot_omd_tilde]] + .rebound_data[[C_dot_o_tilde]]) * .rebound_data[[I_E]] + 
    .rebound_data[[k]] * .rebound_data[[N_dot_hat]] * .rebound_data[[I_E]]
  isos <- isos %>%
    add_iso(indexed = indexed,
            meta = meta,
            graph_type = graph_type,
            iso_name = ReboundTools::rebound_terms$Re_tot,
            colour = graph_params$energy_grid_colour, 
            linewidth = graph_params$energy_grid_linewidth, 
            linetype = graph_params$energy_grid_linetype, 
            x_orig = x_orig, y_orig = y_orig,
            x = x, y = y)
  
  return(isos)
}


#' Create a data frame of iso-expenditure lines
#' 
#' This function creates a data frame of constant expenditure lines 
#' at various rebound stages.
#' The iso lines are described by slope and intercept.
#' 
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param R_alpha_orig,C_dot_s_orig,C_dot_cap_orig,C_dot_omd_orig,C_dot_o_orig See `ReboundTools::orig_vars`.
#' @param G_dot See `ReboundTools::star_vars`.
#' @param R_alpha_star,C_dot_cap_star,C_dot_omd_star,C_dot_o_star See `ReboundTools::star_vars`.
#' @param R_alpha_hat,C_dot_s_hat,C_dot_cap_hat,C_dot_omd_hat,C_dot_o_hat See `ReboundTools::hat_vars`.
#'
#' @return A data frame of iso-expenditure lines for an expenditure path graph.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   iso_expenditure_lines()
iso_expenditure_lines <- function(.rebound_data, 
                           indexed = FALSE,
                           graph_type = ReboundTools::graph_types$expenditure,
                           graph_params = ReboundTools::path_graph_params,
                           
                           R_alpha_orig = ReboundTools::orig_vars$R_alpha_orig,
                           C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig, 
                           C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig, 
                           C_dot_omd_orig = ReboundTools::orig_vars$C_dot_omd_orig, 
                           C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig, 
                           
                           G_dot = ReboundTools::star_vars$G_dot,
                           C_dot_s_hat = ReboundTools::hat_vars$C_dot_s_hat, 
                           R_alpha_star = ReboundTools::star_vars$R_alpha_star,
                           C_dot_cap_star = ReboundTools::star_vars$C_dot_cap_star, 
                           C_dot_omd_star = ReboundTools::star_vars$C_dot_omd_star, 
                           C_dot_o_star = ReboundTools::star_vars$C_dot_o_star,
                           R_alpha_hat = ReboundTools::hat_vars$R_alpha_hat,
                           C_dot_cap_hat = ReboundTools::hat_vars$C_dot_cap_hat, 
                           C_dot_omd_hat = ReboundTools::hat_vars$C_dot_omd_hat, 
                           C_dot_o_hat = ReboundTools::hat_vars$C_dot_o_hat) {
  
  meta <- extract_meta(.rebound_data)
  
  # Iso-expenditure line at the orig point.
  x_orig <- .rebound_data[[C_dot_s_orig]]
  y_orig <- .rebound_data[[R_alpha_orig]]*.rebound_data[[C_dot_cap_orig]] + 
            .rebound_data[[C_dot_omd_orig]] +
            .rebound_data[[C_dot_o_orig]]
  x <- x_orig
  y <- y_orig
  isos <- add_iso(indexed = indexed,
                  meta = meta, 
                  graph_type = graph_type, 
                  iso_name = ReboundTools::rebound_stages$orig,
                  colour = graph_params$expenditure_grid_colour, 
                  linewidth = graph_params$expenditure_grid_linewidth, 
                  linetype = graph_params$expenditure_grid_linetype, 
                  x_orig = x_orig, y_orig = y_orig, 
                  x = x, y = y)
  
  # Iso-expenditure line after expected savings (through the "a" point)
  x <- x - .rebound_data[[G_dot]]
  y <- y
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = graph_type, 
            iso_name = "G_dot",
            colour = graph_params$expenditure_grid_colour, 
            linewidth = graph_params$expenditure_grid_linewidth, 
            linetype = graph_params$expenditure_grid_linetype, 
            x_orig = x_orig, y_orig = y_orig, 
            x = x, y = y)
  
  # Iso-expenditure line after the emplacement effect (through the "star" point)
  # Same x value as iso-expenditure line through the "a" point
  y <- .rebound_data[[R_alpha_star]]*.rebound_data[[C_dot_cap_star]] + 
       .rebound_data[[C_dot_omd_star]] +
       .rebound_data[[C_dot_o_star]]
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = graph_type, 
            iso_name = ReboundTools::rebound_stages$star,
            colour = graph_params$expenditure_grid_colour, 
            linewidth = graph_params$expenditure_grid_linewidth, 
            linetype = graph_params$expenditure_grid_linetype, 
            x_orig = x_orig, y_orig = y_orig, 
            x = x, y = y)

  # Iso-expenditure line after the substitution effect (through the "hat" point)
  x <- .rebound_data[[C_dot_s_hat]]
  y <- .rebound_data[[R_alpha_hat]]*.rebound_data[[C_dot_cap_hat]] + 
       .rebound_data[[C_dot_omd_hat]] +
       .rebound_data[[C_dot_o_hat]]
  isos <- isos %>% 
    add_iso(indexed = indexed, 
            meta = meta, 
            graph_type = graph_type, 
            iso_name = ReboundTools::rebound_stages$hat,
            colour = graph_params$expenditure_grid_colour, 
            linewidth = graph_params$expenditure_grid_linewidth, 
            linetype = graph_params$expenditure_grid_linetype, 
            x_orig = x_orig, y_orig = y_orig, 
            x = x, y = y)
  
  return(isos)
}


#' Create iso-budget (expenditure) lines for a consumption path graph
#' 
#' This function creates a data frame of constant-budget (expenditure) lines 
#' at various rebound stages.
#' The iso lines are described by slope and intercept.
#' 
#' The consumption path graph is _always_ indexed, so there is no `indexed` argument.
#'
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param R_alpha_orig,p_s_orig,q_dot_s_orig,C_dot_cap_orig,C_dot_omd_orig,C_dot_o_orig,M_dot_orig See `ReboundTools::orig_vars`.
#' @param p_s_star,G_dot See `ReboundTools::star_vars`.
#' @param q_dot_s_hat,C_dot_o_hat See `ReboundTools::hat_vars`.
#' @param Delta_q_dot_s_hat,Delta_C_dot_cap_star,Delta_C_dot_omd_star,Delta_C_dot_o_hat See `ReboundTools::Delta_vars`.
#'
#' @return A data frame of iso-budget lines for a consumption path graph.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   iso_budget_lines_cons()
iso_budget_lines_cons <- function(.rebound_data, 
                                  graph_type = ReboundTools::graph_types$consumption,
                                  graph_params = ReboundTools::path_graph_params,
                                  
                                  p_s_orig = ReboundTools::orig_vars$p_s_orig, 
                                  q_dot_s_orig = ReboundTools::orig_vars$q_dot_s_orig,
                                  R_alpha_orig = ReboundTools::orig_vars$R_alpha_orig,
                                  C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig,
                                  C_dot_omd_orig = ReboundTools::orig_vars$C_dot_omd_orig,
                                  C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                                  M_dot_orig = ReboundTools::orig_vars$M_dot_orig,
                                  
                                  p_s_star = ReboundTools::star_vars$p_s_star,
                                  G_dot = ReboundTools::star_vars$G_dot,
                                  
                                  q_dot_s_hat = ReboundTools::hat_vars$q_dot_s_hat,
                                  C_dot_o_hat = ReboundTools::hat_vars$C_dot_o_hat, 
                                  
                                  Delta_q_dot_s_hat = ReboundTools::Delta_vars$Delta_q_dot_s_hat,
                                  Delta_C_dot_cap_star = ReboundTools::Delta_vars$Delta_C_dot_cap_star,
                                  Delta_C_dot_omd_star = ReboundTools::Delta_vars$Delta_C_dot_omd_star,
                                  Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat) {
  
  meta <- extract_meta(.rebound_data)
  
  # Iso-budget line at the orig point.
  # slope_orig <- -p_s_orig * q_dot_s_orig / C_dot_o_orig
  slope_orig <- -.rebound_data[[p_s_orig]] * .rebound_data[[q_dot_s_orig]] / .rebound_data[[C_dot_o_orig]]
  # intercept_orig <- (M_dot_orig - 
  #                    R_alpha_orig*C_dot_cap_orig - 
  #                    C_dot_omd_orig) / C_dot_o_orig
  intercept_orig <- (.rebound_data[[M_dot_orig]] - 
                       .rebound_data[[R_alpha_orig]]*.rebound_data[[C_dot_cap_orig]] - 
                       .rebound_data[[C_dot_omd_orig]]) / 
                     .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(meta = meta, 
                         graph_type = graph_type,
                         line_name = ReboundTools::rebound_stages$orig, 
                         colour = graph_params$cons_grid_colour, 
                         linewidth = graph_params$cons_grid_linewidth, 
                         linetype = graph_params$cons_grid_linetype, 
                         slope = slope_orig,
                         intercept = intercept_orig)
  
  # Iso-budget line at the star point (after emplacement, before substitution)
  # slope_star <- -p_s_star * q_dot_s_orig / C_dot_o_orig
  slope_star <- -.rebound_data[[p_s_star]] * .rebound_data[[q_dot_s_orig]] / .rebound_data[[C_dot_o_orig]]
  # intercept_star <- (M_dot_orig - 
  #                    R_alpha_orig*C_dot_cap_orig - 
  #                    C_dot_omd_orig -
  #                    G_dot) / C_dot_o_orig
  intercept_star <- (.rebound_data[[M_dot_orig]] - 
                       .rebound_data[[R_alpha_orig]]*.rebound_data[[C_dot_cap_orig]] - 
                       .rebound_data[[C_dot_omd_orig]] - 
                       .rebound_data[[G_dot]]) / 
    .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(out, meta = meta, 
                         graph_type = graph_type,
                         line_name = ReboundTools::rebound_stages$star, 
                         colour = graph_params$cons_grid_colour, 
                         linewidth = graph_params$cons_grid_linewidth, 
                         linetype = graph_params$cons_grid_linetype, 
                         slope = slope_star,
                         intercept = intercept_star)
  
  # After this point, the slope doesn't change, but the intercept does.
  
  # Iso-budget line at the hat point (after substitution, before income)
  slope_hat <- slope_star
  # intercept_hat <- (M_dot_orig -
  #                   R_alpha_orig*C_dot_cap_orig - 
  #                   C_dot_omd_orig - 
  #                   G_dot + 
  #                   p_s_star*Delta_q_dot_s_hat + 
  #                   Delta_C_dot_o_hat) / C_dot_o_orig
  intercept_hat <- (.rebound_data[[M_dot_orig]] - 
                      .rebound_data[[R_alpha_orig]]*.rebound_data[[C_dot_cap_orig]] - 
                      .rebound_data[[C_dot_omd_orig]] -
                      .rebound_data[[G_dot]] +
                      .rebound_data[[p_s_star]] * .rebound_data[[Delta_q_dot_s_hat]] + 
                      .rebound_data[[Delta_C_dot_o_hat]]) / 
    .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(out, meta = meta, 
                         graph_type = graph_type,
                         line_name = ReboundTools::rebound_stages$hat, 
                         colour = graph_params$cons_grid_colour, 
                         linewidth = graph_params$cons_grid_linewidth, 
                         linetype = graph_params$cons_grid_linetype, 
                         slope = slope_hat,
                         intercept = intercept_hat)
  
  # Iso-budget line at the bar point (after income, before productivity)
  slope_bar <- slope_hat
  # intercept_bar <- (M_dot_orig - 
  #                   R_alpha_orig*C_dot_cap_orig - 
  #                   C_dot_omd_orig - 
  #                   Delta_C_dot_cap_star - 
  #                   Delta_C_dot_omd_star) / C_dot_o_orig
  intercept_bar <- (.rebound_data[[M_dot_orig]] - 
                      .rebound_data[[R_alpha_orig]]*.rebound_data[[C_dot_cap_orig]] - 
                      .rebound_data[[C_dot_omd_orig]] - 
                      .rebound_data[[Delta_C_dot_cap_star]] - 
                      .rebound_data[[Delta_C_dot_omd_star]]) / 
    .rebound_data[[C_dot_o_orig]]
  out <- add_budget_line(out, meta = meta, 
                         graph_type = graph_type,
                         line_name = ReboundTools::rebound_stages$bar, 
                         colour = graph_params$cons_grid_colour, 
                         linewidth = graph_params$cons_grid_linewidth, 
                         linetype = graph_params$cons_grid_linetype, 
                         slope = slope_bar,
                         intercept = intercept_bar)

  # Add ray from origin through income points.
  # slope_ray <- (C_dot_o_hat / C_dot_o_orig) / (q_dot_s_hat / q_dot_s_orig)
  slope_ray <- (.rebound_data[[C_dot_o_hat]] / .rebound_data[[C_dot_o_orig]]) /
                  (.rebound_data[[q_dot_s_hat]] / .rebound_data[[q_dot_s_orig]])
  intercept_ray <- 0
  out <- add_budget_line(out, meta = meta, 
          graph_type = graph_type, 
          line_name = "ray",
          colour = graph_params$cons_ray_colour, 
          linewidth = graph_params$cons_ray_linewidth, 
          linetype = graph_params$cons_ray_linetype, 
          slope = slope_ray, 
          intercept = intercept_ray)
  return(out)
}


#' Add a budget line to a data frame of budget lines
#' 
#' Adds a budget line to a data frame. 
#' The budget lines are accumulated in rows.
#' 
#' There is usually no need to call this function directly. 
#' Functions like `iso_budget_lines_cons()` `add_iso()` 
#' call `add_budget_line()` internally.
#'
#' @param .DF The data frame to which lines are appended. 
#'            When `NULL`, the default, a new data frame is created and returned.
#'            When not `NULL`, rows for the budget lines are added to the bottom of `.DF`.
#' @param meta A data frame of metadata for the segment to be added. 
#'             This metadata data frame provides the left-most columns of the return value.
#' @param graph_type The type of graph associated with this segment. 
#'                   Default is See `ReboundTools::graph_types$consumption`.
#' @param line_name A name for this budget line 
#' @param colour The colour for this budget line. Default is `ReboundTools::path_graph_params$cons_grid_colour`.
#' @param linewidth The size (width) for this budget line.  Default is `ReboundTools::path_graph_params$cons_grid_linewidth`.
#' @param linetype The line type for this budget line.  Default is `ReboundTools::path_graph_params$cons_grid_linetype`.
#' @param slope The slope of this budget line.
#' @param intercept The intercept of this budget line.
#' @param graph_df_colnames A list of column names in graph data frames. 
#'                          Default is `ReboundTools::graph_df_colnames`.
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
add_budget_line <- function(.DF = NULL, 
                            meta, 
                            graph_type = ReboundTools::graph_types$consumption, 
                            line_name,
                            colour = ReboundTools::path_graph_params$cons_grid_colour, 
                            linewidth = ReboundTools::path_graph_params$cons_grid_linewidth, 
                            linetype = ReboundTools::path_graph_params$cons_grid_linetype,
                            slope, intercept, 
                            graph_df_colnames = ReboundTools::graph_df_colnames) {
  out <- meta %>% 
    dplyr::mutate(
      "{graph_df_colnames$graph_type_col}" := graph_type, 
      "{graph_df_colnames$line_name_col}" := line_name,
      "{graph_df_colnames$colour_col}" := colour, 
      "{graph_df_colnames$linewidth_col}" := linewidth,
      "{graph_df_colnames$linetype_col}" := linetype,
      "{graph_df_colnames$slope_col}" := slope,
      "{graph_df_colnames$intercept_col}" := intercept
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
#' Functions like `iso_energy_lines()` and `iso_expenditure_lines()` 
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
#' @param colour The colour for this iso line. Default is `ReboundTools::path_graph_params$energy_grid_colour`.
#' @param linewidth Line width. Default is `ReboundTools::path_graph_params$energy_grid_size`.
#' @param linetype Line type for this iso line. Default is `ReboundTools::path_graph_params$energy_grid_linetype`. 
#' @param x_orig,y_orig The (x,y) coordinates of the starting point for the path on this graph, 
#'                      used for indexing.
#' @param x,y The (x,y) coordinates of a point on this iso line. 
#'            Slope and intercept are calculated from these values.
#' @param graph_df_colnames A list of column names in graph data frames. 
#'                          Default is `ReboundTools::graph_df_colnames`.
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
add_iso <- function(.DF = NULL, 
                    indexed = FALSE, 
                    meta, 
                    graph_type, 
                    iso_name, 
                    colour = ReboundTools::path_graph_params$energy_grid_colour, 
                    linewidth = ReboundTools::path_graph_params$energy_grid_linewidth, 
                    linetype = ReboundTools::path_graph_params$energy_grid_linetype, 
                    x_orig, y_orig, 
                    x, y,
                    graph_df_colnames = ReboundTools::graph_df_colnames) {
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
  add_budget_line(.DF, 
                  meta = meta, 
                  graph_type = graph_type,
                  line_name = iso_name,
                  colour = colour,
                  linewidth = linewidth,
                  linetype = linetype,
                  slope = slope,
                  intercept = intercept)
}






