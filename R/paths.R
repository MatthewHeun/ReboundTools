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
    dplyr::select(dplyr::any_of(meta_cols))
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
#' @param rebound_segments See `ReboundTools::rebound_segments`.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param k,I_E See `ReboundTools::eeu_base_params`.
#' @param R_alpha_orig,E_dot_s_orig,E_dot_emb_orig,C_dot_omd_orig,C_dot_o_orig See `ReboundTools::orig_vars`.
#' @param S_dot_dev See `ReboundTools::star_vars`.
#' @param Delta_E_dot_emb_star,Delta_C_dot_omd_star,Delta_E_dot_s_hat,Delta_C_dot_o_hat,Delta_E_dot_s_bar,Delta_C_dot_o_bar,N_dot_hat See `ReboundTools::Delta_vars`.
#' @param graph_df_colnames See `ReboundTools::graph_df_colnames`.
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
                         graph_params = ReboundTools::path_graph_params,
                         rebound_segments = ReboundTools::rebound_segments,
                         graph_type = ReboundTools::graph_types$energy,
                         
                         k = ReboundTools::eeu_base_params$k,
                         I_E = ReboundTools::eeu_base_params$I_E,
                         
                         R_alpha_orig = ReboundTools::orig_vars$R_alpha_orig,
                         E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
                         E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig,
                         C_dot_omd_orig = ReboundTools::orig_vars$C_dot_omd_orig,
                         C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                         
                         S_dot_dev = ReboundTools::star_vars$S_dot_dev, 
                         
                         Delta_E_dot_emb_star = ReboundTools::Delta_vars$Delta_E_dot_emb_star,
                         Delta_C_dot_omd_star = ReboundTools::Delta_vars$Delta_C_dot_omd_star,
                         
                         Delta_E_dot_s_hat = ReboundTools::Delta_vars$Delta_E_dot_s_hat,
                         Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                         N_dot_hat = ReboundTools::hat_vars$N_dot_hat,
                         
                         Delta_E_dot_s_bar = ReboundTools::Delta_vars$Delta_E_dot_s_bar,
                         Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar, 
                         
                         graph_df_colnames = ReboundTools::graph_df_colnames) {
  
  # A metadata data frame for all these segments
  meta <- extract_meta(.rebound_data)
  
  # Make each segment individually, 
  # starting from the original point, and using Deltas for everything else.
  
  # Emplacement effect
  
  # S_dot_dev segment for energy graph (dempl)
  x_orig <- .rebound_data[[E_dot_s_orig]]
  y_orig <- .rebound_data[[E_dot_emb_orig]] + 
            (.rebound_data[[C_dot_omd_orig]] + .rebound_data[[C_dot_o_orig]]) * .rebound_data[[I_E]]
  x <- x_orig
  y <- y_orig
  xend <- x_orig - .rebound_data[[S_dot_dev]]
  yend <- y_orig
  paths <- add_segment(indexed = indexed,
                       colour = graph_params$dempl_colour,
                       linewidth = graph_params$dempl_linewidth,
                       linetype = graph_params$dempl_linetype,
                       meta = meta, 
                       graph_type = graph_type, 
                       segment_name = rebound_segments$dempl, 
                       x_orig = x_orig, y_orig = y_orig,
                       x = x, y = y, xend = xend, yend = yend)
  
  # Delta_E_dot_emb_star segment for energy graph (emb)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_E_dot_emb_star]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$emb_colour, 
                linewidth = graph_params$emb_linewidth,
                linetype = graph_params$emb_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = rebound_segments$emb, 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_omd_star*I_E segment for energy graph (md)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_omd_star]] * .rebound_data[[I_E]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$omd_colour, 
                linewidth = graph_params$omd_linewidth,
                linetype = graph_params$omd_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = rebound_segments$omd, 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Substitution effect
  
  # Delta_C_dot_o_hat*I_E segment for energy graph (isub)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_hat]] * .rebound_data[[I_E]]
  paths <- paths %>%
    add_segment(indexed = indexed,
                colour = graph_params$isub_colour,
                linewidth = graph_params$isub_linewidth,
                linetype = graph_params$isub_linetype,
                meta = meta,
                graph_type = graph_type,
                segment_name = rebound_segments$isub,
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)

  # Delta_E_dot_s_hat segment for energy graph (dsub)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_E_dot_s_hat]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$dsub_colour, 
                linewidth = graph_params$dsub_linewidth,
                linetype = graph_params$dsub_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = rebound_segments$dsub, 
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
                linewidth = graph_params$dinc_linewidth,
                linetype = graph_params$dinc_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = rebound_segments$dinc, 
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
                linewidth = graph_params$iinc_linewidth,
                linetype = graph_params$iinc_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = rebound_segments$iinc, 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  # Macro effect (macro)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[k]] * .rebound_data[[N_dot_hat]] * .rebound_data[[I_E]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$macro_colour, 
                linewidth = graph_params$macro_linewidth,
                linetype = graph_params$macro_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = rebound_segments$macro, 
                x_orig = x_orig, y_orig = y_orig,
                x = x, y = y, xend = xend, yend = yend)
  
  if (graph_params$reverse_path_drawing_order) {
    # Reverse the order of segments in the data frame so that
    # arrows will lie upon following segments 
    # when drawn tip-to-tail.
    paths <- paths[nrow(paths):1, ]
  }
  # Add ending arrows to the paths data frame
  paths %>% 
    add_arrows(graph_params = graph_params, graph_df_colnames = graph_df_colnames)
}



#' A data frame of expenditure path segments 
#' 
#' Makes a data frame of segments for an expenditure path graph.
#' Each stage of the rebound process is represented in the data frame.
#'
#' @param .rebound_data A data frame of rebound analysis results, 
#'                      likely created by `rebound_analysis()`.
#' @param indexed A boolean telling whether the rebound path should be indexed to `1` 
#'                at its start.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param rebound_segments See `ReboundTools::rebound_segments`.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param R_alpha_orig,C_dot_s_orig,C_dot_cap_orig,C_dot_omd_orig,C_dot_o_orig See `ReboundTools::orig_vars`.
#' @param G_dot See `ReboundTools::star_vars`.
#' @param R_alpha_star,C_dot_cap_star,Delta_C_dot_omd_star,Delta_C_dot_s_hat,Delta_C_dot_o_hat,Delta_C_dot_s_bar,Delta_C_dot_o_bar See `ReboundTools::Delta_vars`.
#' @param graph_df_colnames See `ReboundTools::graph_df_colnames`.
#' 
#' @return A data frame with cost rebound path segments.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   expenditure_paths()
expenditure_paths <- function(.rebound_data, 
                       indexed = FALSE,
                       graph_params = ReboundTools::path_graph_params,
                       rebound_segments = ReboundTools::rebound_segments,
                       graph_type = ReboundTools::graph_types$expenditure,
                       
                       R_alpha_orig = ReboundTools::orig_vars$R_alpha_orig,
                       C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig, 
                       C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig, 
                       C_dot_omd_orig = ReboundTools::orig_vars$C_dot_omd_orig,
                       C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                       
                       G_dot = ReboundTools::star_vars$G_dot,
                       
                       R_alpha_star = ReboundTools::star_vars$R_alpha_star,
                       C_dot_cap_star = ReboundTools::star_vars$C_dot_cap_star,

                       # Delta_C_dot_cap_star = ReboundTools::Delta_vars$Delta_C_dot_cap_star,
                       Delta_C_dot_omd_star = ReboundTools::Delta_vars$Delta_C_dot_omd_star,
                       Delta_C_dot_s_hat = ReboundTools::Delta_vars$Delta_C_dot_s_hat,
                       Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                       Delta_C_dot_s_bar = ReboundTools::Delta_vars$Delta_C_dot_s_bar,
                       Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar, 
                       
                       graph_df_colnames = ReboundTools::graph_df_colnames) {
  
  # The strategy here is to make each segment individually, 
  # starting from the original point, and using Deltas for everything else.
  
  # A metadata data frame for all these segments
  meta <- extract_meta(.rebound_data)
  
  # Emplacement effect
  
  # G_dot segment for cost graph (dempl)
  x_orig_cost <- .rebound_data[[C_dot_s_orig]]
  y_orig_cost <- .rebound_data[[R_alpha_orig]]*.rebound_data[[C_dot_cap_orig]] +
                 .rebound_data[[C_dot_omd_orig]] + .rebound_data[[C_dot_o_orig]]
  xend <- x_orig_cost - .rebound_data[[G_dot]]
  yend <- y_orig_cost
  paths <- add_segment(indexed = indexed,
                       colour = graph_params$dempl_colour, 
                       linewidth = graph_params$dempl_linewidth,
                       linetype = graph_params$dempl_linetype,
                       meta = meta, 
                       graph_type = graph_type, 
                       segment_name = rebound_segments$dempl, 
                       x_orig = x_orig_cost, y_orig = y_orig_cost,
                       x = x_orig_cost, y = y_orig_cost, xend = xend, yend = yend)
  
  # Delta_C_dot_cap_star segment for cost graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  # yend <- y + .rebound_data[[Delta_C_dot_cap_star]]
  yend <- y + (.rebound_data[[R_alpha_star]]*.rebound_data[[C_dot_cap_star]] - 
               .rebound_data[[R_alpha_orig]]*.rebound_data[[C_dot_cap_orig]])
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$cap_colour, 
                linewidth = graph_params$cap_linewidth,
                linetype = graph_params$cap_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = rebound_segments$cap, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_omd segment for cost graph (iempl)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_omd_star]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$omd_colour, 
                linewidth = graph_params$omd_linewidth,
                linetype = graph_params$omd_linetype,
                meta = meta,
                graph_type = graph_type,
                segment_name = rebound_segments$omd, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Substitution effect
  
  # Delta_C_dot_o_hat segment for cost graph (isub)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_hat]]
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$isub_colour, 
                linewidth = graph_params$isub_linewidth,
                linetype = graph_params$isub_linetype,
                meta = meta, 
                graph_type = graph_type,
                segment_name = rebound_segments$isub, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_s_hat segment for cost graph (dsub)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_C_dot_s_hat]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = indexed,
                colour = graph_params$dsub_colour, 
                linewidth = graph_params$dsub_linewidth,
                linetype = graph_params$dsub_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = rebound_segments$dsub, 
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
                linewidth = graph_params$dinc_linewidth,
                linetype = graph_params$dinc_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = rebound_segments$dinc, 
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
                linewidth = graph_params$iinc_linewidth,
                linetype = graph_params$iinc_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = rebound_segments$iinc, 
                x_orig = x_orig_cost, y_orig = y_orig_cost,
                x = x, y = y, xend = xend, yend = yend)
  if (graph_params$reverse_path_drawing_order) {
    # Reverse the order of segments in the data frame so that
    # arrows will lie upon following segments 
    # when drawn tip-to-tail.
    paths <- paths[nrow(paths):1, ]
  }
  # Add ending arrows to the paths data frame and return
  paths %>% 
    add_arrows(graph_params = graph_params, graph_df_colnames = graph_df_colnames)
}


#' A data frame of consumption paths
#' 
#' Make a data frame of segments 
#' for a consumption path graph.
#' Each stage of the rebound process is represented in the data frame.
#' 
#' The consumption path graph is _always_ indexed, so there is no `indexed` argument.
#'
#' @param .rebound_data A data frame of rebound analysis results, 
#'                      likely created by `rebound_analysis()`.
#' @param graph_params See `ReboundTools::graph_params`.
#' @param rebound_segments See `ReboundTools::rebound_segments`.
#' @param graph_type See `ReboundTools::graph_types`.
#' @param q_dot_s_star,C_dot_o_star See `ReboundTools::star_vars`.
#' @param Delta_q_dot_s_hat,Delta_C_dot_o_hat,Delta_q_dot_s_bar,Delta_C_dot_o_bar See `ReboundTools::Delta_vars`.
#' @param graph_df_colnames See `ReboundTools::graph_df_colnames`.
#'
#' @return A data frame of information for creating consumption path graphs.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   consumption_paths()
consumption_paths <- function(.rebound_data, 
                              graph_params = ReboundTools::path_graph_params,
                              rebound_segments = ReboundTools::rebound_segments,
                              graph_type = ReboundTools::graph_types$consumption,
                              
                              q_dot_s_star = ReboundTools::star_vars$q_dot_s_star, 
                              C_dot_o_star = ReboundTools::star_vars$C_dot_o_star,
                              
                              Delta_q_dot_s_hat = ReboundTools::Delta_vars$Delta_q_dot_s_hat,
                              Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                              Delta_q_dot_s_bar = ReboundTools::Delta_vars$Delta_q_dot_s_bar,
                              Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar,
                              
                              graph_df_colnames = ReboundTools::graph_df_colnames) {
  
  # A metadata data frame for all these segments
  meta <- extract_meta(.rebound_data)
  
  # Starting point
  x_star <- .rebound_data[[q_dot_s_star]]
  y_star <- .rebound_data[[C_dot_o_star]]
    
    
  # Substitution effect.
  
  # Delta_C_dot_o_star segment for consumption path graph (isub)
  x <- x_star
  y <- y_star
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_hat]]
  paths <- add_segment(indexed = TRUE,
                       colour = graph_params$isub_colour, 
                       linewidth = graph_params$isub_linewidth,
                       linetype = graph_params$isub_linetype,
                       meta = meta,
                       graph_type = graph_type, 
                       segment_name = rebound_segments$isub,
                       x_orig = x_star, y_orig = y_star, 
                       x = x, y = y, xend = xend, yend = yend)
  
  # Delta_q_dot_s_star segment for consumption path graph (dsub)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_q_dot_s_hat]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = TRUE,
                colour = graph_params$dsub_colour, 
                linewidth = graph_params$dsub_linewidth,
                linetype = graph_params$dsub_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = rebound_segments$dsub,
                x_orig = x_star, y_orig = y_star, 
                x = x, y = y, xend = xend, yend = yend)
  
  # Income effect
  
  # Delta_q_dot_s_bar segment for consumption path graph (dinc)
  x <- xend
  y <- yend
  xend <- x + .rebound_data[[Delta_q_dot_s_bar]]
  yend <- y
  paths <- paths %>% 
    add_segment(indexed = TRUE,
                colour = graph_params$dinc_colour, 
                linewidth = graph_params$dinc_linewidth,
                linetype = graph_params$dinc_linetype,
                meta = meta, 
                graph_type = graph_type, 
                segment_name = rebound_segments$dinc,
                x_orig = x_star, y_orig = y_star, 
                x = x, y = y, xend = xend, yend = yend)
  
  # Delta_C_dot_o_bar segment for consumption path graph (iinc)
  x <- xend
  y <- yend
  xend <- x
  yend <- y + .rebound_data[[Delta_C_dot_o_bar]]
  paths <- paths %>% 
    add_segment(indexed = TRUE,
                colour = graph_params$iinc_colour,
                linewidth = graph_params$iinc_linewidth,
                linetype = graph_params$iinc_linetype,
                meta = meta,
                graph_type = graph_type, 
                segment_name = rebound_segments$iinc,
                x_orig = x_star, y_orig = y_star, 
                x = x, y = y, xend = xend, yend = yend)
  if (graph_params$reverse_path_drawing_order) {
    # Reverse the order of segments in the data frame so that
    # arrows will lie upon following segments 
    # when drawn tip-to-tail.
    paths <- paths[nrow(paths):1, ]
  }
  # Add ending arrows to the paths data frame
  paths %>% 
    add_arrows(graph_params = graph_params, graph_df_colnames = graph_df_colnames)
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
#' @param linewidth The line width for this segment. Default is `1`.
#' @param linetype The line type for this segment. Default is "solid".
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
                        colour = "black", linewidth = 1, linetype = "solid",
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
      "{graph_df_colnames$linewidth_col}" := linewidth,
      "{graph_df_colnames$linetype_col}" := linetype,
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


#' Add arrow descriptions to a paths data frame
#'
#' @param .paths The data frame to which arrows should be added.
#' @param graph_params See `ReboundTools::path_graph_params`.
#' @param graph_df_colnames See `ReboundTools::graph_df_colnames`.
#' @param rebound_segments See `ReboundTools::rebound_segments`.
#'
#' @return A version of `.paths` with a column for arrow descriptions.
#' 
#' @export
add_arrows <- function(.paths, graph_params, graph_df_colnames, 
                       rebound_segments = ReboundTools::rebound_segments) {
  which_max <- max(which(rebound_segments %in% .paths[[graph_df_colnames$line_name_col]]))
  last_seg <- rebound_segments[[which_max]]
  
  .paths %>% 
    dplyr::left_join(graph_params$which_arrows, by = graph_df_colnames$line_name_col) %>% 
    dplyr::mutate(
      "{graph_df_colnames$end_arrow_col}" := dplyr::case_when(
        .data[[graph_df_colnames$line_name_col]] == last_seg ~ graph_params$last_arrow, 
        TRUE ~ .data[[graph_df_colnames$end_arrow_col]]
      )
    )
}
