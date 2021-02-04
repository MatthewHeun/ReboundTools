#' Extract points from a data frame of paths
#' 
#' Make a data frame of points between energy rebound effects
#' for any type of rebound path graph.
#' Each available stage of the rebound process is represented in the data frame.
#'
#' @param .paths A data frame of rebound paths, 
#'                      likely created by `energy_paths()`, `cost_paths()`, or `prefs_paths()`.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param rebound_stages See `ReboundTools::rebound_stages`.
#' @param rebound_segments See `ReboundTools::rebound_segments`.
#' @param graph_df_colnames See `ReboundTools::graph_df_colnames`.
#' 
#' @return A data frame containing points extracted from paths.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   energy_paths() %>% 
#'   extract_points()
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   cost_paths() %>% 
#'   extract_points()
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   prefs_paths() %>% 
#'   extract_points()
extract_points <- function(.paths, 
                           graph_params = ReboundTools::default_graph_params, 
                           rebound_stages = ReboundTools::rebound_stages, 
                           rebound_segments = ReboundTools::rebound_segments,
                           graph_df_colnames = ReboundTools::graph_df_colnames) {
  
  .paths %>% 
    # Keep only the segments for which a starting point is requested.
    dplyr::filter(.data[[graph_df_colnames$start_point_col]]) %>% 
    dplyr::mutate(
      # Add point names based on segment descriptions
      # and eliminate the line names column.
      "{graph_df_colnames$point_name}" := dplyr::case_when(
        # Set the point name.
        .data[[graph_df_colnames$line_name]] == rebound_segments$dempl ~ rebound_stages$orig, 
        .data[[graph_df_colnames$line_name]] == rebound_segments$isub  ~ rebound_stages$star, 
        .data[[graph_df_colnames$line_name]] == rebound_segments$dinc  ~ rebound_stages$hat,
        .data[[graph_df_colnames$line_name]] == rebound_segments$prod  ~ rebound_stages$bar
      ), 
      # Eliminate unneeded columns
      "{graph_df_colnames$line_name}" := NULL,
      "{graph_df_colnames$linetype_col}" := NULL, 
      "{graph_df_colnames$start_point_col}" := NULL, 
      "{graph_df_colnames$end_arrow_col}" := NULL
    ) %>% 
    # Add shape, size, fill, stroke, and colour for each point.
    dplyr::mutate(
      "{graph_df_colnames$shape}" := graph_params$point_shape, 
      "{graph_df_colnames$size}" := graph_params$point_size, 
      "{graph_df_colnames$stroke}" := graph_params$point_stroke, 
      "{graph_df_colnames$fill}" := .data[[graph_df_colnames$colour]]
    )
}


