#' A data frame of energy rebound points
#' 
#' Make a data frame of points between energy rebound effects
#' for an energy rebound graph.
#' Each stage of the rebound process is represented in the data frame.
#'
#' @param .rebound_data A data frame of rebound analysis results, 
#'                      likely created by `rebound_analysis()`.
#' @param indexed A boolean telling whether the rebound points should be indexed to `1` 
#'                at their start.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param k,I_E See `ReboundTools::eeu_base_params`.
#' @param E_dot_s_orig,E_dot_emb_orig,C_dot_md_orig,C_dot_o_orig See `ReboundTools::orig_vars`.
#' @param S_dot_dev See `ReboundTools::star_vars`.
#' @param Delta_E_dot_emb_star,Delta_C_dot_md_star,Delta_E_dot_s_hat,Delta_C_dot_o_hat,Delta_E_dot_s_bar,Delta_C_dot_o_bar,N_dot_hat See `ReboundTools::Delta_vars`.
#' 
#' @return A data frame with energy rebound path segments.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   energy_points()
energy_points <- function(.rebound_data, 
                          indexed = FALSE,
                          graph_params = ReboundTools::default_graph_params,
                          graph_df_colnames = ReboundTools::graph_df_colnames,
                          graph_type = ReboundTools::graph_types$energy,
                          
                          k = ReboundTools::eeu_base_params$k,
                          I_E = ReboundTools::eeu_base_params$I_E,
                          
                          E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
                          E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig,
                          C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                          C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                          
                          S_dot_dev = ReboundTools::star_vars$S_dot_dev, 
                          
                          Delta_E_dot_emb_star = ReboundTools::Delta_vars$Delta_E_dot_emb_star,
                          Delta_C_dot_md_star = ReboundTools::Delta_vars$Delta_C_dot_md_star,
                          
                          Delta_E_dot_s_hat = ReboundTools::Delta_vars$Delta_E_dot_s_hat,
                          Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                          N_dot_hat = ReboundTools::hat_vars$N_dot_hat,
                          
                          Delta_E_dot_s_bar = ReboundTools::Delta_vars$Delta_E_dot_s_bar,
                          Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar) {
  
  # Get the energy path segments, filter the appropriate rows, and rename things.
  energy_segs <- energy_paths(.rebound_data, 
                              indexed = indexed,
                              graph_params = graph_params,
                              graph_type = graph_type,
                              
                              k = k,
                              I_E = I_E,
                              
                              E_dot_s_orig = E_dot_s_orig,
                              E_dot_emb_orig = E_dot_emb_orig,
                              C_dot_md_orig = C_dot_md_orig,
                              C_dot_o_orig = C_dot_o_orig,
                              
                              S_dot_dev = S_dot_dev, 
                              
                              Delta_E_dot_emb_star = Delta_E_dot_emb_star,
                              Delta_C_dot_md_star = Delta_C_dot_md_star,
                              
                              Delta_E_dot_s_hat = Delta_E_dot_s_hat,
                              Delta_C_dot_o_hat = Delta_C_dot_o_hat,
                              N_dot_hat = N_dot_hat,
                              
                              Delta_E_dot_s_bar = Delta_E_dot_s_bar,
                              Delta_C_dot_o_bar = Delta_C_dot_o_bar) 
  
  # Save the original points
  
  # Grab the rest of the points
  epts <- energy_segs %>% 
    dplyr::mutate(
      "{graph_df_colnames$x_col}" := NULL, 
      "{graph_df_colnames$y_col}" := NULL, 
      "{graph_df_colnames$colour}" := NULL,
      "{graph_df_colnames$size}" := NULL, 
      "{graph_df_colnames$linetype}" := NULL, 
      "{graph_df_colnames$start_point}" := NULL, 
      "{graph_df_colnames$end_arrow}" := NULL, 
    ) %>% 
    dplyr::rename(
      "{graph_df_colnames$x_col}" := .data[[graph_df_colnames$xend_col]],
      "{graph_df_colnames$y_col}" := .data[[graph_df_colnames$yend_col]]
    ) %>% 
    dplyr::mutate(
      "{graph_df_colnames$point_name}" := dplyr::case_when(
        .data[[graph_df_colnames$line_name]] == "Delta_C_dot_md_starI_E" ~ "star", 
        .data[[graph_df_colnames$line_name]] == "Delta_C_dot_o_hatI_E" ~ "hat",
        .data[[graph_df_colnames$line_name]] == "Delta_C_dot_o_barI_E" ~ "bar",
        .data[[graph_df_colnames$line_name]] == "Productivity" ~ "tilde"
      ), 
      "{graph_df_colnames$line_name}" := NULL
    ) %>% 
    tidyr::drop_na(.data[[graph_df_colnames$point_name_col]]) %>% 
    dplyr::mutate(
      "{graph_df_colnames$shape}" := graph_params$point_shape, 
      "{graph_df_colnames$size}" := graph_params$size, 
      "{graph_df_colnames$fill}" := graph_params$fill, 
      "{graph_df_colnames$stroke}" := graph_params$stroke, 
      "{graph_df_colnames$colour}" := graph_params$colour 
    )
  
  # Merge original and rest of the points and add point information.
    
  return(epts)
}


#' Add a point to a data frame
#' 
#' Adds a point to a data frame of line segments.
#' The points are accumulated in rows.
#' 
#' There is usually no need to call this function directly. 
#' Functions like `energy_points()` call `add_points()` internally.
#'
#' @param .DF A data frame that accumulates points. 
#'            When `NULL`, the default, a new data frame is created and returned.
#'            When not `NULL`, rows for the points are added to the bottom of `.DF`.
#' @param indexed A boolean telling whether the points should be indexed to `1` 
#'                at the start of the rebound path.
#' @param meta A data frame of metadata for the points to be added. 
#'             This metadata data frame provides the left-most columns of the return value.
#' @param graph_type The type of graph associated with these points. See `ReboundTools::graph_types`.
#' @param point_name A name for this point.
#' @param shape The line type for this segment. Default is `21` (a filled circle).
#' @param size The size of the filled portion of this point. Default is `1`.
#' @param fill The fill colour of the points. Default is "black".
#' @param stroke The size of the line around this point. Default is `1`.
#' @param colour The colour of the surrounding line for this point. Default is "black".
#' @param x_orig,y_orig The (x,y) coordinates of the starting point for this path, 
#'                      used for indexing.
#' @param x,y The (x,y) coordinates of the starting point for this segment of the path.
#' @param xend,yend The (x,y) coordinates of the ending point for this segment of the path.
#' @param graph_df_colnames A list of column names to use throughout the package.
#'                          Default is `ReboundTools::graph_df_colnames`.
#'
#' @return A version of `.DF` with line segments added at the bottom.
#' 
#' @export
#'
#' @examples
#' meta <- tibble::tibble(Case = "Test case")
#' add_point(indexed = FALSE, meta = meta, graph_type = "Test type", 
#'           point_name = "Test point", 
#'           x_orig = 10, y_orig = 10, 
#'           x = 20, y = 30)
add_point <- function(.DF = NULL, 
                      indexed, meta, graph_type, point_name, 
                      shape = 21, size = 1, fill = "black", stroke = 1, colour = "black", 
                      x_orig, y_orig, x, y,
                      graph_df_colnames = ReboundTools::graph_df_colnames) {
  if (indexed) {
    x <- x/x_orig
    y <- y/y_orig 
  }
  out <- meta %>% 
    dplyr::mutate(
      "{graph_df_colnames$graph_type_col}" := graph_type, 
      "{graph_df_colnames$point_name_col}" := point_name,
      "{graph_df_colnames$shape}" := shape,
      "{graph_df_colnames$size_col}" := size,
      "{graph_df_colnames$fill_col}" := fill,
      "{graph_df_colnames$stroke_col}" := stroke,
      "{graph_df_colnames$colour_col}" := colour, 
      "{graph_df_colnames$x_col}" := x, 
      "{graph_df_colnames$y_col}" := y
    )
  if (is.null(.DF)) {
    return(out)
  }
  .DF %>% 
    dplyr::bind_rows(out)
}
