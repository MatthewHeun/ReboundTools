#' A data frame of energy rebound points
#' 
#' Make a data frame of points between energy rebound effects
#' for an energy rebound graph.
#' Each stage of the rebound process is represented in the data frame.
#'
#' @param .energy_paths A data frame of energy paths, 
#'                      likely created by `energy_paths()`.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param rebound_stages See `ReboundTools::rebound_stages`.
#' @param rebound_segments See `ReboundTools::rebound_segments`.
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
                          graph_params = ReboundTools::default_graph_params, 
                          rebound_stages = ReboundTools::rebound_stages, 
                          rebound_segments = ReboundTools::rebound_segments) {
  
  # Save the original points
  orig_points <- .energy_paths %>% 
    dplyr::filter(.data[[graph_df_colnames$line_name_col]] == rebound_segments$dempl) %>% 
    dplyr::mutate(
      "{graph_df_colnames$xend_col}" := NULL,
      "{graph_df_colnames$yend_col}" := NULL,
    )
  
  # Grab the rest of the points
  other_points <- .energy_paths %>% 
    dplyr::filter(.data[[graph_df_colnames$line_name_col]] != rebound_segments$dempl) %>% 
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
        .data[[graph_df_colnames$line_name]] == rebound_segments$dempl ~ rebound_stages$orig, 
        .data[[graph_df_colnames$line_name]] == rebound_segments$md ~ rebound_stages$star, 
        .data[[graph_df_colnames$line_name]] == rebound_segments$dsub ~ rebound_stages$hat,
        .data[[graph_df_colnames$line_name]] == rebound_segments$iinc ~ rebound_stages$bar,
        .data[[graph_df_colnames$line_name]] == rebound_segments$prod ~ rebound_stages$tilde
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


