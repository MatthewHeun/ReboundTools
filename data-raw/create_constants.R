# This script creates constants and saves them in the right locations.
# If there are any changes to these constants, 
# source this script before building the package.

library(magrittr)
library(ReboundTools)


#
# Details of EEU data tables
# 

eeu_data_table <- list(eeu_data_sheet = "EEU data")
usethis::use_data(eeu_data_table, overwrite = TRUE)


#
# Units information
# 

rebound_units <- list(energy_si = "MJ", 
                      time_unit = "year", 
                      currency_unit = "$",
                      currency_unit_latex = "\\$",
                      unitless = "-",
                      unitless_latex = "--", 
                      leading_delta_pattern = "^Delta_",
                      surround_left = "[", 
                      surround_right = "]")
usethis::use_data(rebound_units, overwrite = TRUE)


#
# Names of key rebound variables in LaTeX format
# 

latex_key_analysis_vars <- data.frame(
  var_name = c("eta_engr_units", "eta", "p_s", "q_dot_s", "E_dot_s",
               "E_dot_emb", "C_dot_s", "C_dot_cap",
               "C_dot_md", "C_dot_o", "N_dot", "M_dot"), 
  latex_var_name = c("$\\eta$", "$\\eta$", "$p_s$", "$\\dot{q}_s$", "$\\dot{E}_s$",
                     "$\\dot{E}_{emb}$", "$\\dot{C}_s$", "$\\dot{C}_{cap}$",
                     "$\\dot{C}_{md}$", "$\\dot{C}_o$", "$\\dot{N}$", "$\\dot{M}$")
)
usethis::use_data(latex_key_analysis_vars, overwrite = TRUE)


#
# Names of key rebound variables
# 

key_analysis_vars <- latex_key_analysis_vars$var_name
names(key_analysis_vars) <- key_analysis_vars
usethis::use_data(key_analysis_vars, overwrite = TRUE)


#
# Names of stages
# 

rebound_stages <- list(orig  = "orig", 
                       star  = "star",  # After emplacement effect
                       hat   = "hat",   # After subsituttion effect
                       bar   = "bar",   # After income effect
                       tilde = "tilde") # After productivity effect
usethis::use_data(rebound_stages, overwrite = TRUE)


#
# Names of stages in LaTeX format
# 

latex_rebound_stages <- data.frame(
  stage = ReboundTools::rebound_stages %>% unlist() %>% unname(),
  latex_stage_name = c("$\\circ$ (orig)", 
                       "$*$ (star)", 
                       "$\\wedge$ (hat)",
                       "$-$ (bar)",
                       "$\\sim$ (tilde)")
)
usethis::use_data(latex_rebound_stages, overwrite = TRUE)


#
# Names of segments in path data frames.
# Also gives sequential order in the path graphs.
# 

rebound_segments <- list(dempl = "dempl",
                         emb = "emb",
                         cap = "cap",
                         md = "md", 
                         isub = "isub", 
                         dsub = "dsub",
                         dinc = "dinc", 
                         iinc = "iinc", 
                         prod = "prod")
usethis::use_data(rebound_segments, overwrite = TRUE)

#
# Delta variables
# 

Delta_vars <- expand.grid(key_analysis_vars, rebound_stages) %>% 
  magrittr::set_names(c("key_analysis_vars", "rebound_stages")) %>% 
  dplyr::mutate(
    Delta_vars = paste0("Delta_", .data[["key_analysis_vars"]], "_", .data[["rebound_stages"]])
  ) %>% 
  dplyr::select(.data[["Delta_vars"]]) %>% 
  dplyr::filter(!endsWith(.data[["Delta_vars"]], paste0("_", ReboundTools::rebound_stages[["orig"]]))) %>% 
  unlist()
Delta_vars <- Delta_vars %>% 
  magrittr::set_names(Delta_vars) %>% 
  as.list()
usethis::use_data(Delta_vars, overwrite = TRUE)


#
# Names of base parameters for a rebound analysis.
# 

eeu_base_params <- list(reference = "Reference",
                        case = "Case", 
                        original = "Original", 
                        upgrade = "Upgrade", 
                        service_unit = "service_unit",
                        energy_engr_unit = "energy_engr_unit",
                        MJ_engr_unit = "MJ/energy_engr_unit",
                        I_E = "I_E", 
                        k = "k", 
                        p_E_engr_units = "p_E_engr_units",
                        e_qs_ps_UC = "e_qs_ps_UC", 
                        e_qs_M = "e_qs_M", 
                        e_qo_M = "e_qo_M")
usethis::use_data(eeu_base_params, overwrite = TRUE)


#
# Names of calculated variables at the "orig" stage.
# 

orig_vars <- list(p_E = "p_E",
                  q_dot_s_orig = "q_dot_s_orig",
                  M_dot_orig = "M_dot_orig", 
                  C_cap_orig = "C_cap_orig", 
                  t_own_orig = "t_own_orig", 
                  C_dot_md_orig = "C_dot_md_orig", 
                  eta_engr_units_orig = "eta_engr_units_orig", 
                  eta_orig = "eta_orig", 
                  E_dot_s_orig = "E_dot_s_orig",
                  C_dot_cap_orig = "C_dot_cap_orig",
                  p_s_orig = "p_s_orig", 
                  C_dot_s_orig = "C_dot_s_orig",
                  C_dot_o_orig = "C_dot_o_orig",
                  f_Cs_orig = "f_Cs_orig",
                  e_qs_ps = "e_qs_ps",
                  e_qo_ps = "e_qo_ps", 
                  sigma = "sigma",
                  E_emb_orig = "E_emb_orig",
                  t_life_orig = "t_life_orig",
                  E_dot_emb_orig = "E_dot_emb_orig", 
                  N_dot_orig = "N_dot_orig")
usethis::use_data(orig_vars, overwrite = TRUE)


#
# Names of calculated variables at the "star" stage.
# 

star_vars <- list(C_cap_star	= "C_cap_star", 
                  t_own_star = "t_own_star", 
                  C_dot_md_star = "C_dot_md_star", 
                  E_emb_star = "E_emb_star",
                  t_life_star = "t_life_star",
                  eta_engr_units_star = "eta_engr_units_star", 
                  eta_star = "eta_star",
                  eta_ratio = "eta_ratio", 
                  S_dot_dev = "S_dot_dev",
                  G_dot = "G_dot", 
                  p_s_star = "p_s_star",
                  q_dot_s_star = "q_dot_s_star",
                  C_dot_cap_star = "C_dot_cap_star",
                  E_dot_emb_star = "E_dot_emb_star",
                  C_dot_s_star = "C_dot_s_star", 
                  M_dot_star = "M_dot_star",
                  N_dot_star = "N_dot_star",
                  C_dot_o_star = "C_dot_o_star", 
                  E_dot_s_star = "E_dot_s_star")
usethis::use_data(star_vars, overwrite = TRUE)


#
# Names of calculated variables at the "hat" stage.
# 

hat_vars <- list(eta_engr_units_hat = "eta_engr_units_hat", 
                 eta_hat = "eta_hat",
                 p_s_hat = "p_s_hat",
                 C_dot_cap_hat = "C_dot_cap_hat",
                 C_dot_md_hat = "C_dot_md_hat",
                 E_dot_emb_hat = "E_dot_emb_hat",
                 M_dot_hat = "M_dot_hat",
                 q_dot_s_hat = "q_dot_s_hat",
                 E_dot_s_hat = "E_dot_s_hat",
                 C_dot_s_hat = "C_dot_s_hat",
                 C_dot_o_hat = "C_dot_o_hat", 
                 N_dot_hat = "N_dot_hat",
                 M_dot_hat_prime = "M_dot_hat_prime")
usethis::use_data(hat_vars, overwrite = TRUE)


#
# Names of calculated variables at the "bar" stage.
# 

bar_vars <- list(eta_engr_units_bar = "eta_engr_units_bar",
                 eta_bar = "eta_bar",
                 p_s_bar = "p_s_bar",
                 C_dot_cap_bar = "C_dot_cap_bar",
                 C_dot_md_bar = "C_dot_md_bar",
                 E_dot_emb_bar = "E_dot_emb_bar",
                 M_dot_bar = "M_dot_bar",
                 q_dot_s_bar = "q_dot_s_bar",
                 E_dot_s_bar = "E_dot_s_bar",
                 C_dot_s_bar = "C_dot_s_bar",
                 C_dot_o_bar = "C_dot_o_bar", 
                 N_dot_bar = "N_dot_bar")
usethis::use_data(bar_vars, overwrite = TRUE)


#
# Names of calculated variables at the "tilde" stage.
# 

tilde_vars <- list(eta_engr_units_tilde = "eta_engr_units_tilde",
                   eta_tilde = "eta_tilde",
                   p_s_tilde = "p_s_tilde",
                   C_dot_cap_tilde = "C_dot_cap_tilde",
                   C_dot_md_tilde = "C_dot_md_tilde",
                   E_dot_emb_tilde = "E_dot_emb_tilde",
                   M_dot_tilde = "M_dot_tilde",
                   q_dot_s_tilde = "q_dot_s_tilde",
                   E_dot_s_tilde = "E_dot_s_tilde",
                   C_dot_s_tilde = "C_dot_s_tilde",
                   C_dot_o_tilde = "C_dot_o_tilde", 
                   N_dot_tilde = "N_dot_tilde")
usethis::use_data(tilde_vars, overwrite = TRUE)


#
# Names of rebound terms calculated after all Delta variables are calculated.
# 

rebound_terms <- list(Re_dempl = "Re_dempl",
                      Re_emb = "Re_emb",
                      Re_cap = "Re_cap",
                      Re_md = "Re_md",
                      Re_empl = "Re_empl",
                      Re_dsub = "Re_dsub", 
                      Re_isub = "Re_isub",
                      Re_sub = "Re_sub",
                      Re_dinc = "Re_dinc", 
                      Re_iinc = "Re_iinc", 
                      Re_inc = "Re_inc",
                      Re_prod = "Re_prod", 
                      Re_dir = "Re_dir",
                      Re_indir = "Re_indir",
                      Re_tot = "Re_tot")
usethis::use_data(rebound_terms, overwrite = TRUE)


#
# Names of rebound terms that are aggregates.
# 

rebound_terms_agg <- list(rebound_terms$Re_empl,
                          rebound_terms$Re_sub,
                          rebound_terms$Re_inc,
                          rebound_terms$Re_d,
                          rebound_terms$Re_i,
                          rebound_terms$Re_tot)
usethis::use_data(rebound_terms_agg, overwrite = TRUE)



#
# LaTeX names of rebound terms.
# 

latex_rebound_terms <- list(Re_dempl = "$Re_{dempl}$",
                            Re_emb = "$Re_{emb}$",
                            Re_cap = "$Re_{cap}$",
                            Re_md = "$Re_{md}$",
                            Re_empl = "$Re_{empl}$",
                            Re_dsub = "$Re_{dsub}$", 
                            Re_isub = "$Re_{isub}$",
                            Re_sub = "$Re_{sub}$",
                            Re_dinc = "$Re_{dinc}$", 
                            Re_iinc = "$Re_{iinc}$", 
                            Re_inc = "$Re_{inc}$",
                            Re_prod = "$Re_{prod}$", 
                            Re_dir = "$Re_{dir}$",
                            Re_indir = "$Re_{indir}$",
                            Re_tot = "$Re_{tot}$")
usethis::use_data(latex_rebound_terms, overwrite = TRUE)


#
# Graph types
# 

graph_types <- list(energy = "Energy",
                    cost = "Cost",
                    preferences = "Preferences")
usethis::use_data(graph_types, overwrite = TRUE)


#
# Graph data frame column names
# 

graph_df_colnames <- list(colour_col = "colour", 
                          size_col = "size", 
                          shape_col = "shape",
                          fill_col = "fill",
                          stroke_col = "stroke",
                          linetype_col = "linetype",
                          graph_type_col = "graph_type",
                          line_name_col = "line_name",
                          point_name_col = "point_name",
                          slope_col = "slope", 
                          intercept_col = "intercept",
                          x_col = "x", 
                          y_col = "y", 
                          xend_col = "xend",
                          yend_col = "yend", 
                          qs1_qs0_col = "qs1_qs0", 
                          Co1_Co0_col = "Co1_Co0", 
                          f_Cs_orig_col = "f_Cs_orig",
                          sigma_col = "sigma", 
                          start_point_col = "start_point",
                          end_arrow_col = "end_arrow", 
                          Re_names = "Re_names", 
                          Re_values = "Re_values", 
                          y_names_col = "y_names",
                          y_vals_col = "y_vals")
usethis::use_data(graph_df_colnames, overwrite = TRUE)


#
# Default path graph parameters
# 

# Note: These colours match the colours in the rebound paper
# emb_colour <- rgb(245, 194, 193, maxColorValue = 255, alpha = 255)
# sub_colour <- rgb(250, 224, 195, maxColorValue = 255, alpha = 255)
# inc_colour <- rgb(205, 253, 197, maxColorValue = 255, alpha = 255)
# prod_colour <- rgb(191, 192, 250, maxColorValue = 255, alpha = 255)

# These colours approximately match the colours in the rebound paper,
# but they are a little bolder.
# empl_colour <- "pink2"
# sub_colour <- "peachpuff"
# inc_colour <- "palegreen2"
# prod_colour <- "slateblue1"


# These colours are obtained from the viridis colour scale

v_colours <- viridis::viridis(4, option = "plasma", begin = 0.01, end = 0.95)
# v_colours <- viridis::viridis(4, direction = -1)
# v_colours <- viridis::viridis(4, direction = -1, end = 0.93)
# v_colours <- viridis::viridis(4, direction = -1, end = 0.85)
empl_colour <- v_colours[1]
sub_colour <- v_colours[2]
inc_colour <- v_colours[3]
prod_colour <- v_colours[4]
tot_colour <- "black"

arr_style <- grid::arrow(angle = 20, 
                         length = grid::unit(0.1, "inches"),
                         type = "closed")

path_graph_params <- list(# Points on paths
                          which_points = tibble::tibble("{graph_df_colnames$point_name_col}" := unlist(rebound_stages), 
                                "{graph_df_colnames$start_point_col}" := c(TRUE, TRUE, TRUE, TRUE, FALSE)),
                          last_point = FALSE,
                          point_shape = 19,
                          point_size = 1,
                          point_stroke = 1,
                          
                          # Arrows on paths
                          which_arrows = tibble::tibble("{graph_df_colnames$line_name_col}" := unlist(rebound_segments), 
                                                        "{graph_df_colnames$end_arrow_col}" := c(rep.int(FALSE, 8), FALSE)),
                          last_arrow = TRUE,
                          arrow_style = arr_style,
                          
                          # Path colours
                          dempl_colour = empl_colour, 
                          emb_colour = empl_colour,
                          cap_colour = empl_colour,
                          md_colour = empl_colour, 
                          empl_colour = empl_colour,
                          isub_colour = sub_colour, 
                          dsub_colour = sub_colour,
                          sub_colour = sub_colour, 
                          dinc_colour = inc_colour,
                          iinc_colour = inc_colour, 
                          inc_colour = inc_colour,
                          prod_colour = prod_colour, 
                          dir_colour = tot_colour, 
                          indir_colour = tot_colour, 
                          tot_colour = tot_colour,
                          
                          # Path line widths
                          dempl_size = 1, 
                          emb_size = 1.5,
                          cap_size = 1.5,
                          md_size = 1, 
                          empl_size = 1,
                          isub_size = 1,
                          dsub_size = 1,
                          sub_size = 1,
                          dinc_size = 1,
                          iinc_size = 1, 
                          inc_size = 1,
                          prod_size = 1,
                          dir_size = 1,
                          indir_size = 1,
                          tot_size = 2,
                          
                          # Path linetypes
                          dempl_linetype = "solid",
                          emb_linetype = "11",
                          cap_linetype = "11",
                          md_linetype = "solid", 
                          empl_linetype = "solid",
                          dsub_linetype = "solid",
                          isub_linetype = "solid", 
                          sub_linetype = "solid",
                          dinc_linetype = "solid",
                          iinc_linetype = "solid", 
                          sinc_linetype = "solid",
                          prod_linetype = "solid",
                          dir_linetype = "solid",
                          indir_linetype = "solid",
                          tot_linetype = "solid",
                          
                          # Path line end and join
                          lineend = "round", 
                          linejoin = "round",
                          
                          # Layering controls
                          
                          # Set order for paths drawn in same layer
                          reverse_path_drawing_order = FALSE,
                          # Draw points on top of paths (or not)
                          points_atop_paths = TRUE,
                          
                          # Grid line colours
                          energy_grid_colour = "black",
                          zero_perc_rebound_grid_colour = "black",
                          hundred_perc_rebound_grid_colour = "black",
                          energy_rebound_lines_colour = "black",
                          cost_grid_colour = "black",
                          prefs_grid_colour = "black",
                          prefs_ray_colour = "black",
                          prefs_indiff_grid_colour = "black",
                          
                          # Grid line sizes
                          energy_grid_size = 0.1,
                          zero_perc_rebound_grid_size = 0.3,
                          hundred_perc_rebound_grid_size = 0.3,
                          energy_rebound_lines_size = 0.1,
                          cost_grid_size = 0.3,
                          prefs_grid_size = 0.1,
                          prefs_ray_size = 0.1,
                          prefs_indiff_grid_size = 0.5,
                          
                          # Grid line types
                          energy_grid_linetype = "solid",
                          zero_perc_rebound_grid_linetype = "solid",
                          hundred_perc_rebound_grid_linetype = "solid",
                          energy_rebound_lines_linetype = "solid",
                          cost_grid_linetype = "solid",
                          prefs_grid_linetype = "solid",
                          prefs_ray_linetype = "solid",
                          prefs_indiff_grid_linetype = "solid", 
                          n_indiff_curve_points = 200,
                          qs_qs0_lower = 0.1,
                          qs_qs0_upper = 10)
usethis::use_data(path_graph_params, overwrite = TRUE)


#
# Default sensitivity graph parameters
# 

sens_graph_params <- list(# Base condition points on graphs
                          orig_point_shape = 16,
                          orig_point_size = 2,
                          orig_point_stroke = 0.5,
                          orig_point_colour = "black",
                          orig_point_fill = "black",
                          
                          # Rebound effect colours
                          dempl_colour = empl_colour, 
                          emb_colour = empl_colour,
                          cap_colour = empl_colour,
                          md_colour = empl_colour, 
                          empl_colour = empl_colour, 
                          dsub_colour = sub_colour,
                          isub_colour = sub_colour, 
                          sub_colour = sub_colour, 
                          dinc_colour = inc_colour,
                          iinc_colour = inc_colour, 
                          inc_colour = inc_colour, 
                          prod_colour = prod_colour, 
                          dir_colour = tot_colour,
                          indir_colour = tot_colour,
                          tot_colour = tot_colour,
                          
                          # Line widths for rebound effects
                          dempl_size = 0.5, 
                          emb_size = 0.5,
                          cap_size = 0.5,
                          md_size = 0.5, 
                          empl_size = 0.5,
                          dsub_size = 0.5,
                          isub_size = 0.5,
                          sub_size = 0.5,
                          dinc_size = 0.5,
                          iinc_size = 0.5,
                          inc_size = 0.5,
                          prod_size = 0.5,
                          dir_size = 0.5,
                          indir_size = 0.5,
                          tot_size = 1,
                          
                          # linetypes for rebound effects
                          dempl_linetype = "solid",
                          emb_linetype = "11",
                          cap_linetype = "11",
                          md_linetype = "41", 
                          empl_linetype = "solid",
                          dsub_linetype = "solid",
                          isub_linetype = "11",
                          sub_linetype = "solid",
                          dinc_linetype = "solid",
                          iinc_linetype = "11", 
                          inc_linetype = "solid",
                          prod_linetype = "solid",
                          dir_linetype = "solid", 
                          indir_linetype = "11", 
                          tot_linetype = "solid",
                          
                          # line end and join for sensitivity graphs
                          lineend = "round", 
                          linejoin = "round",
                          # Draw points on top of paths (or not)
                          include_base_condition_points = TRUE,
                          points_atop_paths = TRUE, 
                          
                          # Adjust miscellaneous features of the graph
                          include_x_axis = FALSE,
                          use_latex_legend = FALSE
                          )
usethis::use_data(sens_graph_params, overwrite = TRUE)

#
# Parametric analysis point types
# 

parametric_analysis_point_types <- list(point_type_colname = "point_type",
                                        orig = "orig", 
                                        sweep = "sweep")
usethis::use_data(parametric_analysis_point_types, overwrite = TRUE)
