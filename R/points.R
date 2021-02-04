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
  lp <- ".last_point"
  which_max <- max(which(rebound_segments %in% .paths[[graph_df_colnames$line_name_col]]))
  last_seg <- rebound_segments[[which_max]]
  
  last_point <- .paths %>% 
    dplyr::filter(.data[[graph_df_colnames$line_name_col]] == last_seg) %>% 
    dplyr::mutate(
      "{graph_df_colnames$x_col}" := NULL, 
      "{graph_df_colnames$y_col}" := NULL, 
      "{graph_df_colnames$line_name_col}" := lp
    ) %>% 
    dplyr::rename(
      "{graph_df_colnames$x_col}" := .data[[graph_df_colnames$xend]], 
      "{graph_df_colnames$y_col}" := .data[[graph_df_colnames$yend]]
    )
  other_points <- .paths %>% 
    dplyr::mutate(
      "{graph_df_colnames$xend_col}" := NULL, 
      "{graph_df_colnames$yend_col}" := NULL
    )
  
  dplyr::bind_rows(last_point, other_points) %>% 
    dplyr::mutate(
      # Add point names based on line names
      # and eliminate the line names column.
      "{graph_df_colnames$point_name_col}" := dplyr::case_when(
        # Set the point name.
        .data[[graph_df_colnames$line_name]] == rebound_segments$dempl ~ rebound_stages$orig, 
        .data[[graph_df_colnames$line_name]] == rebound_segments$isub  ~ rebound_stages$star, 
        .data[[graph_df_colnames$line_name]] == rebound_segments$dinc  ~ rebound_stages$hat,
        .data[[graph_df_colnames$line_name]] == rebound_segments$prod  ~ rebound_stages$bar, 
        .data[[graph_df_colnames$line_name]] == lp                     ~ rebound_stages$tilde, 
      ), 
      # Eliminate unneeded columns
      "{graph_df_colnames$line_name_col}" := NULL,
      "{graph_df_colnames$linetype_col}" := NULL, 
      "{graph_df_colnames$start_point_col}" := NULL, 
      "{graph_df_colnames$end_arrow_col}" := NULL
    ) %>% 
    dplyr::left_join(graph_params$which_points, by = graph_df_colnames$point_name_col) %>% 
    # Add shape, size, fill, stroke, and colour for each point.
    dplyr::mutate(
      "{graph_df_colnames$shape_col}"  := graph_params$point_shape, 
      "{graph_df_colnames$size_col}"   := graph_params$point_size, 
      "{graph_df_colnames$stroke_col}" := graph_params$point_stroke, 
      "{graph_df_colnames$fill_col}"   := .data[[graph_df_colnames$colour]]
    )
}


