
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
                           C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig, 
                           C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig, 
                           C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig, 
                           C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig
                           ) {
  
  meta <- extract_meta(.rebound_data)
  
  # Iso-cost line at the orig point.
  x_orig <- .rebound_data[[C_dot_s_orig]]
  y_orig <- .rebound_data[[C_dot_cap_orig]] + .rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]
  x <- x_orig
  y <- y_orig
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
add_iso <- function(.DF = NULL, indexed = FALSE, meta, graph_type, iso_name, 
        colour = ReboundTools::graph_colours$grid, size = 0.5, linetype = "solid", 
        x_orig, y_orig, 
        x, y) {
  if (indexed) {
    x <- x/x_orig
    y <- y/y_orig
  }
  out <- meta %>% 
    dplyr::mutate(
      graph_type = graph_type, 
      iso_name = iso_name,
      colour = colour, 
      size = size,
      linetype = linetype,
      m = -1, 
      b = x + y
    )
  if (is.null(.DF)) {
    return(out)
  }
  .DF %>% 
    dplyr::bind_rows(out)
}


