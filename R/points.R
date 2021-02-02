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
#'   energy_paths() %>% 
#'   energy_points()
energy_points <- function(.energy_paths, 
                          graph_params = ReboundTools::default_graph_params) {
  
  # Save the original points
  orig_points <- .energy_paths %>% 
    dplyr::filter(.data[[graph_df_colnames$line_name_col]] == "S_dot_dev") %>% 
    dplyr::mutate(
      "{graph_df_colnames$xend_col}" := NULL,
      "{graph_df_colnames$yend_col}" := NULL,
    )
  
  # Grab the rest of the points
  other_points <- .energy_paths %>% 
    dplyr::filter(.data[[graph_df_colnames$line_name_col]] != "S_dot_dev") %>% 
    dplyr::mutate(
      "{graph_df_colnames$x_col}" := NULL, 
      "{graph_df_colnames$y_col}" := NULL, 
    ) %>% 
    dplyr::rename(
      "{graph_df_colnames$x_col}" := .data[[graph_df_colnames$xend_col]],
      "{graph_df_colnames$y_col}" := .data[[graph_df_colnames$yend_col]]
    ) 
  
  # Combine original and other points
  epoints <- dplyr::bind_rows(orig_points, other_points) %>% 
    dplyr::mutate(
      # Eliminate unneeded columns
      "{graph_df_colnames$colour}" := NULL,
      "{graph_df_colnames$size}" := NULL, 
      "{graph_df_colnames$linetype}" := NULL, 
      "{graph_df_colnames$start_point}" := NULL, 
      "{graph_df_colnames$end_arrow}" := NULL, 
      # Add point names based on segment descriptions
      # and eliminate the line names column.
      "{graph_df_colnames$point_name}" := dplyr::case_when(
        .data[[graph_df_colnames$line_name]] == "S_dot_dev" ~ "orig", 
        .data[[graph_df_colnames$line_name]] == "Delta_C_dot_md_starI_E" ~ "star", 
        .data[[graph_df_colnames$line_name]] == "Delta_C_dot_o_hatI_E" ~ "hat",
        .data[[graph_df_colnames$line_name]] == "Delta_C_dot_o_barI_E" ~ "bar",
        .data[[graph_df_colnames$line_name]] == "Productivity" ~ "tilde"
      ), 
      "{graph_df_colnames$line_name}" := NULL
    ) %>% 
    # Keep only those rows where the point_name is specified.
    tidyr::drop_na(.data[[graph_df_colnames$point_name_col]]) %>% 
    # Add shape, size, fill, stroke, and colour for each point.
    dplyr::mutate(
      "{graph_df_colnames$shape}" := graph_params$point_shape, 
      "{graph_df_colnames$size}" := graph_params$size, 
      "{graph_df_colnames$fill}" := graph_params$fill, 
      "{graph_df_colnames$stroke}" := graph_params$stroke, 
      "{graph_df_colnames$colour}" := graph_params$colour 
    )

  return(epoints)
}


