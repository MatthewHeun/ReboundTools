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
  
  
  # 
  # 
  # 
  # # Get the name of the first segment from the top row of the data frame.
  # first_segment <- .paths[[graph_df_colnames$line_name_col]][[1]]
  # 
  # if (first_segment == rebound_segments$isub) {
  #   # We have a paths data frame for the preferences graph
  #   first_point <- rebound_stages$star
  # } else {
  #   # We have a .paths data frame for an energy graph or a cost graph
  #   first_point <- rebound_stages$orig
  # }
  # # Get the name of the last segment from the bottom row of the data frame.
  # last_segment <- .paths[[graph_df_colnames$line_name_col]][[nrow(.paths)]]
  # 
  # # Save the first stage points
  # first_points <- .paths %>% 
  #   dplyr::filter(.data[[graph_df_colnames$line_name_col]] == first_segment) %>% 
  #   dplyr::mutate(
  #     "{graph_df_colnames$xend_col}" := NULL,
  #     "{graph_df_colnames$yend_col}" := NULL,
  #   )
  # 
  # # Grab the rest of the points
  # other_points <- .paths %>% 
  #   dplyr::filter(.data[[graph_df_colnames$line_name_col]] != first_segment) %>% 
  #   dplyr::mutate(
  #     "{graph_df_colnames$x_col}" := NULL, 
  #     "{graph_df_colnames$y_col}" := NULL, 
  #   ) %>% 
  #   dplyr::rename(
  #     "{graph_df_colnames$x_col}" := .data[[graph_df_colnames$xend_col]],
  #     "{graph_df_colnames$y_col}" := .data[[graph_df_colnames$yend_col]]
  #   ) 
  # # # Eliminate last data points if not wanted
  # # if (!graph_params$show_last_point) {
  # #   other_points <- other_points %>% 
  # #     dplyr::filter(.data[[graph_df_colnames$line_name_col]] != last_segment)
  # # }
  # # 
  # # # Include first point or not.
  # # out <- other_points
  # # if (graph_params$show_first_open_circle) {
  # #   out <- dplyr::bind_rows(first_points, other_points)
  # # }
  # 
  # # Clean up and return
  # out <- dplyr::bind_rows(first_points, other_points) %>% 
  #   dplyr::mutate(
  #     # Eliminate unneeded columns
  #     "{graph_df_colnames$colour}" := NULL,
  #     "{graph_df_colnames$size}" := NULL, 
  #     "{graph_df_colnames$linetype}" := NULL, 
  #     "{graph_df_colnames$start_point}" := NULL, 
  #     "{graph_df_colnames$end_arrow}" := NULL, 
  #     # Add point names based on segment descriptions
  #     # and eliminate the line names column.
  #     "{graph_df_colnames$point_name}" := dplyr::case_when(
  #       # When the first segment is indirect substitution and we hit the isub row,
  #       # Set the point name.
  #       .data[[graph_df_colnames$line_name]] == rebound_segments$isub & first_segment == rebound_segments$isub ~ rebound_stages$star, 
  #       .data[[graph_df_colnames$line_name]] == rebound_segments$dempl ~ rebound_stages$orig, 
  #       .data[[graph_df_colnames$line_name]] == rebound_segments$md    ~ rebound_stages$star, 
  #       .data[[graph_df_colnames$line_name]] == rebound_segments$dsub  ~ rebound_stages$hat,
  #       .data[[graph_df_colnames$line_name]] == rebound_segments$iinc  ~ rebound_stages$bar,
  #       .data[[graph_df_colnames$line_name]] == rebound_segments$prod  ~ rebound_stages$tilde
  #     ), 
  #     "{graph_df_colnames$line_name}" := NULL
  #   ) %>% 
  #   # Keep only those rows where the point_name is specified.
  #   tidyr::drop_na(.data[[graph_df_colnames$point_name_col]]) %>% 
  #   # Add shape, size, fill, stroke, and colour for each point.
  #   dplyr::mutate(
  #     "{graph_df_colnames$shape}" := graph_params$point_shape, 
  #     "{graph_df_colnames$size}" := graph_params$point_size, 
  #     "{graph_df_colnames$stroke}" := graph_params$point_stroke, 
  #     "{graph_df_colnames$colour}" := dplyr::case_when(
  #       .data[[graph_df_colnames$point_name]] == rebound_stages$orig ~ graph_params$dempl_colour,
  #       .data[[graph_df_colnames$point_name]] == rebound_stages$star ~ graph_params$isub_colour,
  #       .data[[graph_df_colnames$point_name]] == rebound_stages$hat ~ graph_params$dinc_colour,
  #       .data[[graph_df_colnames$point_name]] == rebound_stages$bar ~ graph_params$prod_colour, 
  #       .data[[graph_df_colnames$point_name]] == rebound_stages$tilde ~ NA_character_, 
  #       TRUE ~ NA_character_
  #     ),
  #     "{graph_df_colnames$fill}" := .data[[graph_df_colnames$colour]]
  #   )
  # 
  # return(out)
}


