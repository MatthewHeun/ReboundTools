#' Extract a metadata data frame from rebound data
#' 
#' Many functions in `ReboundTools` use the metadata from energy efficiency upgrade
#' parameters as a starting point for rebound paths or other graph creation.
#' This function provides a way to quickly extract that metadata.
#'
#' @param .rebound_data The data frame from which metadata is to be extracted.
#' @param meta_cols A vector of column names for metadata columns. Default is `c(reference, case, original, upgrade)`.
#' @param reference,case,original,upgrade Names of metadata columns. See `ReboundTools::eeu_base_params`.
#'
#' @return A data frame containing only metadata columns. 
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   extract_meta()
extract_meta <- function(.rebound_data, 
                         meta_cols = c(reference, case, original, upgrade),
                         reference = ReboundTools::eeu_base_params$reference,
                         case = ReboundTools::eeu_base_params$case, 
                         original = ReboundTools::eeu_base_params$original, 
                         upgrade = ReboundTools::eeu_base_params$upgrade) {
  .rebound_data %>% 
    dplyr::select(dplyr::all_of(meta_cols))
}


#' A data frame of energy rebound paths
#' 
#' Make a data frame of segments 
#' for an energy rebound graph.
#' Each stage of the rebound process is represented in the data frame.
#'
#' @param .rebound_data A data frame of rebound analysis results, 
#'                      likely created by `rebound_analysis()`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
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
#'   energy_paths()
energy_paths <- function(.rebound_data, 
                         indexed = FALSE,
                         graph_params = ReboundTools::default_graph_params,
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
  
  # A metadata data frame for all these segments
  meta <- extract_meta(.rebound_data)
  
  # Make each segment individually, 
  # starting from the original point, and using Deltas for everything else.
  
  # Emplacement effect
  
  # S_dot_dev segment for energy graph (dempl)
  x_orig <- .rebound_data[[E_dot_s_orig]]
  y_orig <- .rebound_data[[E_dot_emb_orig]] + 
    (.rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]) * .rebound_data[[I_E]]
  x <- x_orig
  y <- y_orig
  xend <- x_orig - .rebound_data[[S_dot_dev]]
  yend <- y_orig
  paths <- add_segment(indexed = indexed,
                       colour = graph_params$dempl_colour,
                       size = graph_params$dempl_size,
                       linetype = graph_params$dempl_linetype,
                       meta = meta, 
                       graph_type = graph_type, 
                       segment_name = S_dot_dev, 
                       x_orig = x_orig, y_orig = y_orig,
                       x = x, y = y, xend = xend, yend = yend,
                       start_point = TRUE)
  
  # Delta_E_dot_emb_star segment for energy graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_E_dot_emb_star]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$emb_colour, 
                size = graph_params$emb_size,
                linetype = graph_params$emb_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = Delta_E_dot_emb_star, 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_md_star*I_E segment for energy graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_md_star]] * .rebound_data[[I_E]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$md_colour, 
                size = graph_params$md_size,
                linetype = graph_params$md_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = paste0(Delta_C_dot_md_star, I_E), 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Substitution effect
  
  # Delta_E_dot_s_hat segment for energy graph (dsub)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_E_dot_s_hat]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$dsub_colour, 
                size = graph_params$dsub_size,
                linetype = graph_params$dsub_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = Delta_E_dot_s_hat, 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_hat*I_E segment for energy graph (isub)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_hat]] * .rebound_data[[I_E]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$isub_colour, 
                size = graph_params$isub_size,
                linetype = graph_params$isub_linetype,
                meta = meta,
                graph_type = graph_type,
                segment_name = paste0(Delta_C_dot_o_hat, I_E), 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Income effect
  
  # Delta_E_dot_s_bar segment for energy graph (dinc)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_E_dot_s_bar]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$dinc_colour, 
                size = graph_params$dinc_size,
                linetype = graph_params$dinc_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = Delta_E_dot_s_bar, 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_bar*I_E segment for energy graph (iinc)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_bar]] * .rebound_data[[I_E]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$iinc_colour, 
                size = graph_params$iinc_size,
                linetype = graph_params$iinc_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = paste0(Delta_C_dot_o_bar, I_E), 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Productivity effect (prod)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[k]] * .rebound_data[[N_dot_hat]] * .rebound_data[[I_E]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$prod_colour, 
                size = graph_params$prod_size,
                linetype = graph_params$prod_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = "Productivity", 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend,
                end_arrow = TRUE)
  
  return(paths)
}



#' Make a data frame of segments 
#' for a cost rebound graph.
#' Each stage of the rebound process is represented in the data frame.
#'
#' @param .rebound_data A data frame of rebound analysis results, 
#'                      likely created by `rebound_analysis()`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param C_dot_s_orig,C_dot_cap_orig,C_dot_md_orig,C_dot_o_orig See `ReboundTools::orig_vars`.
#' @param G_dot See `ReboundTools::star_vars`.
#' @param Delta_C_dot_cap_star,Delta_C_dot_md_star,Delta_C_dot_s_hat,Delta_C_dot_o_hat,Delta_C_dot_s_bar,Delta_C_dot_o_bar See `ReboundTools::Delta_vars`.
#' 
#' @return A data frame with cost rebound path segments.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   cost_paths()
cost_paths <- function(.rebound_data, 
                       indexed = FALSE,
                       graph_params = ReboundTools::default_graph_params,
                       graph_type = ReboundTools::graph_types$cost,

                       C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig, 
                       C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig, 
                       C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                       C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                       
                       G_dot = ReboundTools::star_vars$G_dot,

                       Delta_C_dot_cap_star = ReboundTools::Delta_vars$Delta_C_dot_cap_star,
                       Delta_C_dot_md_star = ReboundTools::Delta_vars$Delta_C_dot_md_star,
                       Delta_C_dot_s_hat = ReboundTools::Delta_vars$Delta_C_dot_s_hat,
                       Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                       Delta_C_dot_s_bar = ReboundTools::Delta_vars$Delta_C_dot_s_bar,
                       Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar) {
  
  # The strategy here is to make each segment individually, 
  # starting from the original point, and using Deltas for everything else.
  
  # A metadata data frame for all these segments
  meta <- extract_meta(.rebound_data)
  
  # Emplacement effect
  
  # G_dot segment for cost graph (demple)
  x_orig_cost <- .rebound_data[[C_dot_s_orig]]
  y_orig_cost <- .rebound_data[[C_dot_cap_orig]] + .rebound_data[[C_dot_md_orig]] + .rebound_data[[C_dot_o_orig]]
  xend <- x_orig_cost - .rebound_data[[G_dot]]
  yend <- y_orig_cost
  paths <- add_segment(indexed = indexed,
                       colour = graph_params$dempl_colour, 
                       size = graph_params$dempl_size,
                       linetype = graph_params$dempl_linetype,
                       meta = meta, 
                       graph_type = graph_type, 
                       segment_name = G_dot, 
                       x_orig = x_orig_cost, y_orig = y_orig_cost,
                       x = x_orig_cost, y = y_orig_cost, xend = xend, yend = yend, 
                       start_point = TRUE)
  
  # Delta_C_dot_cap_star segment for cost graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_cap_star]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$cap_colour, 
                size = graph_params$cap_size,
                linetype = graph_params$cap_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = Delta_C_dot_cap_star, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_md segment for cost graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_md_star]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$md_colour, 
                size = graph_params$md_size,
                linetype = graph_params$md_linetype,
                meta = meta,
                graph_type = graph_type,
                segment_name = Delta_C_dot_md_star, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Substitution effect
  
  # Delta_C_dot_s_hat segment for cost graph (dsub)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_C_dot_s_hat]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$dsub_colour, 
                size = graph_params$dsub_size,
                linetype = graph_params$dsub_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = Delta_C_dot_s_hat, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_hat segment for cost graph (isub)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_hat]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$isub_colour, 
                size = graph_params$isub_size,
                linetype = graph_params$isub_linetype,
                meta = meta, 
                graph_type = graph_type,
                segment_name = Delta_C_dot_o_hat, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Income effect
  
  # Delta_C_dot_s_bar segment for cost graph (dinc)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_C_dot_s_bar]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$dinc_colour, 
                size = graph_params$dinc_size,
                linetype = graph_params$dinc_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = Delta_C_dot_s_bar, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_bar segment for cost graph (iinc)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_bar]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$iinc_colour, 
                size = graph_params$iinc_size,
                linetype = graph_params$iinc_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = Delta_C_dot_o_bar, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend, 
                end_arrow = TRUE)
  
  return(paths)
}


#' A data frame of preferences paths
#' 
#' Make a data frame of segments 
#' for a preferences graph.
#' Each stage of the rebound process is represented in the data frame.
#' 
#' The preferences graph is _always_ indexed, so there is no `indexed` argument.
#'
#' @param .rebound_data A data frame of rebound analysis results, 
#'                      likely created by `rebound_analysis()`.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param q_dot_s_star,C_dot_o_star See `ReboundTools::star_vars`.
#' @param Delta_q_dot_s_hat,Delta_C_dot_o_hat,Delta_q_dot_s_bar,Delta_C_dot_o_bar See `ReboundTools::Delta_vars`.
#'
#' @return A data frame of information for creating preference graphs.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   prefs_paths()
prefs_paths <- function(.rebound_data, 
                        graph_params = ReboundTools::default_graph_params,
                        graph_type = ReboundTools::graph_types$preferences,
                        
                        q_dot_s_star = ReboundTools::star_vars$q_dot_s_star, 
                        C_dot_o_star = ReboundTools::star_vars$C_dot_o_star,
                        
                        Delta_q_dot_s_hat = ReboundTools::Delta_vars$Delta_q_dot_s_hat,
                        Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                        Delta_q_dot_s_bar = ReboundTools::Delta_vars$Delta_q_dot_s_bar,
                        Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar) {
  
  # A metadata data frame for all these segments
  meta <- extract_meta(.rebound_data)
  
  # Starting point
  x_star <- .rebound_data[[q_dot_s_star]]
  y_star <- .rebound_data[[C_dot_o_star]]
    
    
  # Substitution effect.
  
  # Delta_C_dot_o_star segment for prefs graph (isub)
  x <- x_star
  y <- y_star
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_hat]]
  paths <- add_segment(indexed = TRUE,
                       colour = graph_params$isub_colour, 
                       size = graph_params$isub_size,
                       linetype = graph_params$isub_linetype,
                       meta = meta,
                       graph_type = graph_type, 
                       segment_name = Delta_C_dot_o_hat,
                       x_orig = x_star, y_orig = y_star, 
                       x = x, y = y, xend = xend, yend = yend, 
                       start_point = TRUE)
  
  # Delta_q_dot_s_star segment for prefs graph (dsub)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_q_dot_s_hat]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = TRUE,
                colour = graph_params$dsub_colour, 
                size = graph_params$dsub_size,
                linetype = graph_params$dsub_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = Delta_q_dot_s_hat,
                x_orig = x_star, y_orig = y_star, 
                x = x, y = y, xend = xend, yend = yend)
  
  # Income effect
  
  # Delta_q_dot_s_bar segment for prefs graph (dinc)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_q_dot_s_bar]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = TRUE,
                colour = graph_params$dinc_colour, 
                size = graph_params$dinc_size,
                linetype = graph_params$dinc_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = Delta_q_dot_s_bar,
                x_orig = x_star, y_orig = y_star, 
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_bar segment for prefs graph (iinc)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_bar]]
  paths <- paths %>% 
    add_segment(indexed = TRUE,
                colour = graph_params$iinc_colour,
                size = graph_params$iinc_size,
                linetype = graph_params$iinc_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = Delta_C_dot_o_bar,
                x_orig = x_star, y_orig = y_star, 
                x = x, y = y, xend = xend, yend = yend, 
                end_arrow = TRUE)
  
  return(paths)
}


#' Add a line segment to a data frame
#' 
#' Adds a line segment to a data frame of line segments.
#' The line segments are accumulated in rows.
#' 
#' There is usually no need to call this function directly. 
#' Functions like `energy_paths()` call `add_segment()` internally.
#'
#' @param .DF A data frame that accumulates line segments. 
#'            When `NULL`, the default, a new data frame is created and returned.
#'            When not `NULL`, rows for the segment are added to the bottom of `.DF`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
#' @param meta A data frame of metadata for the segment to be added. 
#'             This metadata data frame provides the left-most columns of the return value.
#' @param graph_type The type of graph associated with this segment. See `ReboundTools::graph_types`.
#' @param segment_name A name for this segment. 
#' @param colour The colour for this segment. Default is "black".
#' @param size The size (width) for this segment. Default is `1`.
#' @param linetype The line type for this segment. Default is "solid".
#' @param start_point A boolean that tells whether this segment would be plotted with a starting point. Default is `FALSE`.
#' @param end_arrow A boolean that tells whether this segment would be plotted with an ending arrow. Default is `FALSE`.
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
#' add_segment(indexed = FALSE, meta = meta, graph_type = "Test type", 
#'             segment_name = "Test segment", 
#'             x_orig = 10, y_orig = 10, 
#'             x = 20, y = 30, xend = 40, yend = 50)
add_segment <- function(.DF = NULL, 
                        indexed, meta, graph_type, segment_name, 
                        colour = "black", size = 1, linetype = "solid",
                        start_point = FALSE, end_arrow = FALSE,
                        x_orig, y_orig, x, y, xend, yend, 
                        graph_df_colnames = ReboundTools::graph_df_colnames) {
  if (indexed) {
    x <- x/x_orig
    y <- y/y_orig 
    xend <- xend/x_orig
    yend <- yend/y_orig
  }
  out <- meta %>% 
    dplyr::mutate(
      "{graph_df_colnames$graph_type_col}" := graph_type, 
      "{graph_df_colnames$line_name_col}" := segment_name,
      "{graph_df_colnames$colour_col}" := colour, 
      "{graph_df_colnames$size_col}" := size,
      "{graph_df_colnames$linetype_col}" := linetype,
      "{graph_df_colnames$start_point_col}" := start_point,
      "{graph_df_colnames$end_arrow_col}" := end_arrow,
      "{graph_df_colnames$x_col}" := x, 
      "{graph_df_colnames$y_col}" := y, 
      "{graph_df_colnames$xend_col}" := xend, 
      "{graph_df_colnames$yend_col}" := yend
    )
  if (is.null(.DF)) {
    return(out)
  }
  .DF %>% 
    dplyr::bind_rows(out)
}
