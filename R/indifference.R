#' Indifference curve lines
#' 
#' This function calculates indifference curves 
#' to be drawn on the consumption path graph.
#'
#' The consumption path graph is _always_ indexed, so there is no `indexed` argument.
#'
#' @param .rebound_data A data frame of rebound information, 
#'                      likely created by `rebound_analysis()`.
#' @param graph_type See `ReboundTools::graph_types`. Default is `ReboundTools::graph_types$consumption`.
#' @param graph_params Parameters that control the appearance of the graph. Default is `ReboundTools::path_graph_params`.
#' @param q_dot_s_orig,C_dot_o_orig,f_Cs_orig,sigma See `ReboundTools::orig_vars`.
#' @param q_dot_s_hat See `ReboundTools::hat_vars`.
#' @param q_dot_s_bar,C_dot_o_bar See `ReboundTools::bar_vars`.
#'
#' @return A data frame of indifference curves for a consumption path graph.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   indifference_lines()
indifference_lines <- function(.rebound_data, 
                               graph_type = ReboundTools::graph_types$consumption,
                               graph_params = ReboundTools::path_graph_params,
                               
                               q_dot_s_orig = ReboundTools::orig_vars$q_dot_s_orig,
                               C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                               f_Cs_orig = ReboundTools::orig_vars$f_Cs_orig,
                               sigma = ReboundTools::orig_vars$sigma,
                               
                               q_dot_s_hat = ReboundTools::hat_vars$q_dot_s_hat,

                               q_dot_s_bar = ReboundTools::bar_vars$q_dot_s_bar, 
                               C_dot_o_bar = ReboundTools::bar_vars$C_dot_o_bar) {
  
  meta <- extract_meta(.rebound_data)
  
  # Original data.  
  qs0 <- .rebound_data[[q_dot_s_orig]]
  Co0 <- .rebound_data[[C_dot_o_orig]]
  f_Cs0 <- .rebound_data[[f_Cs_orig]]
  sigma_val <- .rebound_data[[sigma]]
  
  # Indifference curve at the orig point (same as the star point, same as the hat point)
  qs1 <- qs0
  Co1 <- Co0
  # Make sure we include the hat point in our curve.
  qs2 <- .rebound_data[[q_dot_s_hat]]
  
  qs1_qs0 <- qs1/qs0
  Co1_Co0 <- Co1/Co0
  qs2_qs0 <- qs2/qs0
  
  icurves <- add_indifference_curve(meta = meta,
                                    graph_type = graph_type,
                                    line_name = ReboundTools::rebound_stages$orig,
                                    qs1_qs0 = qs1_qs0,
                                    Co1_Co0 = Co1_Co0,
                                    qs2_qs0 = qs2_qs0,
                                    f_Cs_orig = f_Cs0,
                                    sigma = sigma_val, 
                                    graph_params = graph_params)
  
  # Indifference curve at the bar point (after income effect)
  qs1 <- .rebound_data[[q_dot_s_bar]]
  Co1 <- .rebound_data[[C_dot_o_bar]]
  qs1_qs0 <- qs1/qs0
  Co1_Co0 <- Co1/Co0
  icurves <- icurves %>%
    add_indifference_curve(meta = meta,
                           graph_type = graph_type,
                           line_name = ReboundTools::rebound_stages$hat,
                           qs1_qs0 = qs1_qs0,
                           Co1_Co0 = Co1_Co0, 
                           qs2_qs0 = NULL,
                           f_Cs_orig = f_Cs0,
                           sigma = sigma_val, 
                           graph_params = graph_params)
  return(icurves)
}


#' Add an indifference curve to a data frame
#' 
#' Adds an indifference curve to a data frame of indifference curves
#' to be plotted on a consumption path graph.
#' The indifference curves are accumulated in rows.
#' 
#' The consumption path graph is _always_ indexed, so there is no `indexed` argument.
#' 
#' This function finds a reasonable range over which to sweep the `qs_qs0` variable
#' when generating the indifference curve, depending on the value of `qs2_qs0`.
#' When `qs2_qs0` is `NULL` (the default), the curve is generated over the range
#' `grqph_params$qs_qs0_lower * qs1_qs0` to `grqph_params$qs_qs0_upper * qs1_qs0`.
#' When `qs2_qs0` is non-`NULL`, the curve is generated over the range
#' `grqph_params$qs_qs0_lower * min(qs1_qs0, qs2_qs0)` to
#' `grqph_params$qs_qs0_upper * max(qs1_qs0, qs2_qs0)`.
#'
#' @param .DF A data frame that accumulates indifference curves for consumption path graphs. 
#'            When `NULL`, the default, a new data frame is created and returned.
#'            When not `NULL`, rows for the curves are added to the bottom of `.DF`.
#' @param meta A data frame of metadata for the segment to be added. 
#'             This metadata data frame provides the left-most columns of the return value.
#' @param graph_type The graph type for the indifference curve.
#'                   Default is `ReboundTools::graph_types$consumption`.
#' @param line_name A name for this indifference curve.
#' @param qs1_qs0,Co1_Co0 The (x,y) coordinates of a point on this indifference curve.
#' @param qs2_qs0 A second x value at which a a point on the indifference curve should be calculated. Default is `NULL`.
#' @param f_Cs_orig The ratio of spending on the energy service to 
#'                  the sum of initial spending on the energy service and other goods.
#' @param sigma The elasticity of substitution between spending on the energy service and spending on other goods.
#' @param graph_params Parameters that control the appearance of this graph.
#' @param eeu_base_params See `ReboundTools::eeu_base_params`.
#' @param graph_df_colnames A list of column names to use throughout the package.
#'                          Default is `ReboundTools::graph_df_colnames`.
#'
#' @return A version of `.DF` with new indifference curves added at the bottom.
#' 
#' @export
#'
#' @examples
#' meta <- tibble::tibble(Case = "Test case")
#' add_indifference_curve(meta = meta, line_name= "Test indifference curve",
#'                        qs1_qs0 = 2, Co1_Co0 = 3, f_Cs_orig = 0.0001, sigma = 0.2)
add_indifference_curve <- function(.DF = NULL, 
                                   meta, 
                                   graph_type = ReboundTools::graph_types$consumption, 
                                   line_name, 
                                   qs1_qs0, Co1_Co0, qs2_qs0 = NULL, f_Cs_orig, sigma,
                                   graph_params = ReboundTools::path_graph_params,
                                   eeu_base_params = ReboundTools::eeu_base_params,
                                   graph_df_colnames = ReboundTools::graph_df_colnames) {
  # Calculate x values at which indifference curve should be evaluated.
  if (is.null(qs2_qs0)) {
    min_qs <- qs1_qs0
    max_qs <- qs1_qs0
  } else {
    min_qs <- lapply(X = list(qs1_qs0, qs2_qs0), FUN = min) %>% unlist()
    max_qs <- lapply(X = list(qs1_qs0, qs2_qs0), FUN = max) %>% unlist()
  }
  x_vals <- Map(f = geom_seq, 
                from = graph_params$qs_qs0_lower*min_qs, 
                to = graph_params$qs_qs0_upper*max_qs, 
                n = graph_params$n_indiff_curve_points)
  # Be sure to include the qs1_qs0 and qs2_qs0 points, too.
  x_vals <- Map(f = c, x_vals, qs1_qs0)
  if (!is.null(qs2_qs0)) {
    x_vals <- Map(f = c, x_vals, qs2_qs0)
  }
  x_vals <- lapply(X = x_vals, FUN = sort)
  cases <- meta[[eeu_base_params$case]]
  x_df <- Map(f = function(cas, x_v){data.frame(cas, x_v)}, cas = cases, x_v = x_vals) %>% 
    dplyr::bind_rows() %>% 
    magrittr::set_names(c(eeu_base_params$case, graph_df_colnames$x_col)) %>% 
    # Eliminate unnecessary repeated rows
    unique()
  
  out <- meta %>% 
    dplyr::mutate(
      "{graph_df_colnames$graph_type_col}" := graph_type, 
      "{graph_df_colnames$line_name_col}" := line_name,
      "{graph_df_colnames$colour_col}" := graph_params$cons_indiff_grid_colour, 
      "{graph_df_colnames$size_col}" := graph_params$cons_indiff_grid_size,
      "{graph_df_colnames$linetype_col}" := graph_params$cons_indiff_grid_linetype, 
      "{graph_df_colnames$qs1_qs0_col}" := qs1_qs0,
      "{graph_df_colnames$Co1_Co0_col}" := Co1_Co0,
      "{graph_df_colnames$f_Cs_orig_col}" := f_Cs_orig, 
      "{graph_df_colnames$sigma_col}" := sigma
    ) %>% 
    dplyr::full_join(x_df, by = eeu_base_params$case) %>% 
    dplyr::mutate(
      # Now add y values via indifference_func()
      "{graph_df_colnames$y_col}" := indifference_func(qs_qs0 = .data[[graph_df_colnames$x_col]], 
                                                       qs1_qs0 = .data[[graph_df_colnames$qs1_qs0_col]], 
                                                       Co1_Co0 = .data[[graph_df_colnames$Co1_Co0_col]], 
                                                       f_Cs_orig = .data[[graph_df_colnames$f_Cs_orig_col]], 
                                                       sigma = .data[[graph_df_colnames$sigma_col]])
    ) %>% 
    # Get rid of any NaN values in the y column
    dplyr::filter(!is.nan(.data[[graph_df_colnames$y_col]]))
  if (is.null(.DF)) {
    return(out)
  }
  .DF %>% 
    dplyr::bind_rows(out)
}


#' An indifference curve
#' 
#' This function gives points along an indifference curve in 
#' (q_dot_s/q_dot_s_orig, C_dot_o/C_dot_o_orig) space.
#' The indifference curve assumes CES utility. 
#' The equation of the indifference curve is
#' `u_dot/u_dot_orig = [f_Cs*(q_dot_s/q_dot_s_orig)^rho + (1-f_Cs)*(C_dot_o/C_dot_o_orig)^rho]^(1/rho)`.
#' 
#' This function is vectorized.
#'
#' @param qs_qs0 The ratio `q_dot_s/q_dot_s_orig`. Sweeping this variable (x) gives
#'               the indifference curve parameterized by the other arguments.
#' @param qs1_qs0 The x coordinate of a point on this indifference curve.
#' @param Co1_Co0 The y coordinate of a point on this indifference curve.
#' @param f_Cs_orig The fraction of original spending on the energy service relative to the sum of energy service and other goods spending, calculated by `C_dot_s_orig / (C_dot_s_orig + C_dot_o_orig)`.
#' @param sigma The elasticity of substitution between the energy service and other goods.
#' @param rho The exponent in the CES utility function. Default is `(sigma-1)/sigma`.
#'
#' @return The value of `C_dot_o/C_dot_o_orig`, given values of remaining arguments.
#' 
#' @export
#'
#' @examples
#' qs1_qs0 <- 1
#' Co1_Co0 <- 1
#' sigma <- 0.3
#' f_Cs_orig <- 0.01
#' DF <- data.frame(x = seq(0.5, 1.5, by = 0.1)) %>% 
#'   dplyr::mutate(
#'     y = indifference_func(x, qs1_qs0 = qs1_qs0, Co1_Co0 = Co1_Co0, 
#'                           f_Cs_orig = f_Cs_orig, sigma = sigma)
#'   )
#' DF
indifference_func <- function(qs_qs0, qs1_qs0, Co1_Co0, f_Cs_orig, sigma, rho = (sigma-1)/sigma) {
  term1 <- f_Cs_orig/(1 - f_Cs_orig)
  term2 <- qs1_qs0^rho
  term3 <- qs_qs0^rho
  term4 <- Co1_Co0^rho
  twominusthree <- term2 - term3
  (term1*twominusthree + term4)^(1/rho)
}


#' Generate a geometric sequence
#'
#' @param from The first value in the sequence.
#' @param to The final value in the sequence.
#' @param n The number of points to include in the sequence.
#' 
#' @return A vector of numbers.
#' 
#' @export
#'
#' @examples
#' geom_seq(from = 0.01, to = 10, n = 20)
#' geom_seq(from = 10, to = 20, n = 30)
geom_seq <- function(from, to, n) {
  assertthat::assert_that(n >= 2, msg = "n >= 2 required in geom_seq()")
  by = (to/from) ^ (1/(n-1))
  from * by^(0:(n-1))
}








