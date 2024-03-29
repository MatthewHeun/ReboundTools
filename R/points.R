#' Extract points from a data frame of paths
#' 
#' Make a data frame of points between energy rebound effects
#' for any type of rebound path graph.
#' Each available stage of the rebound process is represented in the data frame.
#'
#' @param .paths A data frame of rebound paths, 
#'                      likely created by `energy_paths()`, `expenditure_paths()`, or `consumption_paths()`.
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
#'   expenditure_paths() %>% 
#'   extract_points()
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   consumption_paths() %>% 
#'   extract_points()
extract_points <- function(.paths, 
                           graph_params = ReboundTools::path_graph_params, 
                           rebound_stages = ReboundTools::rebound_stages, 
                           rebound_segments = ReboundTools::rebound_segments,
                           graph_df_colnames = ReboundTools::graph_df_colnames) {
  ls <- ".last_seg"
  lp <- ".last_point"
  # Find that last segement.
  max_which <- max(which(rebound_segments %in% .paths[[graph_df_colnames$line_name_col]]))
  last_seg <- rebound_segments[[max_which]]
  
  # Set x and y of last point to xend and yend of last segment.
  last_point <- .paths %>% 
    dplyr::filter(.data[[graph_df_colnames$line_name_col]] == last_seg) %>% 
    dplyr::mutate(
      "{graph_df_colnames$x_col}" := NULL, 
      "{graph_df_colnames$y_col}" := NULL, 
      "{graph_df_colnames$line_name_col}" := ls
    ) %>% 
    dplyr::rename(
      "{graph_df_colnames$x_col}" := dplyr::all_of(graph_df_colnames$xend), 
      "{graph_df_colnames$y_col}" := dplyr::all_of(graph_df_colnames$yend)
    )
  # Get the other points.
  other_points <- .paths %>% 
    dplyr::mutate(
      "{graph_df_colnames$xend_col}" := NULL, 
      "{graph_df_colnames$yend_col}" := NULL
    )
  
  # Add the last_point and other_points together and operate on them all.
  dplyr::bind_rows(last_point, other_points) %>% 
    dplyr::mutate(
      # Add point names based on line names
      # and eliminate the line names column.
      "{graph_df_colnames$point_name_col}" := dplyr::case_when(
        # Set the point name.
        .data[[graph_df_colnames$line_name]] == rebound_segments$dempl  ~ rebound_stages$orig, 
        .data[[graph_df_colnames$line_name]] == rebound_segments$isub   ~ rebound_stages$star, 
        .data[[graph_df_colnames$line_name]] == rebound_segments$dinc   ~ rebound_stages$hat,
        .data[[graph_df_colnames$line_name]] == rebound_segments$macro  ~ rebound_stages$bar, 
        .data[[graph_df_colnames$line_name]] == ls                      ~ lp
      ), 
      # Eliminate unneeded columns
      "{graph_df_colnames$line_name_col}" := NULL,
      "{graph_df_colnames$linetype_col}" := NULL, 
      "{graph_df_colnames$start_point_col}" := NULL, 
      "{graph_df_colnames$end_arrow_col}" := NULL
    ) %>% 
    dplyr::left_join(graph_params$which_points, by = graph_df_colnames$point_name_col) %>% 
    # Set the status of the last point.
    dplyr::mutate(
      "{graph_df_colnames$start_point_col}" := dplyr::case_when(
        .data[[graph_df_colnames$point_name_col]] == lp ~ graph_params$last_point,
        TRUE ~ .data[[graph_df_colnames$start_point_col]]
      ), 
      # Add shape, size, fill, stroke, and colour for each point.
      "{graph_df_colnames$shape_col}"  := graph_params$point_shape,
      "{graph_df_colnames$size_col}"   := graph_params$point_size,
      "{graph_df_colnames$stroke_col}" := graph_params$point_stroke,
      "{graph_df_colnames$fill_col}"   := .data[[graph_df_colnames$colour]]
    )
}



