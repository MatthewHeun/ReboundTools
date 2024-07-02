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
                      time_unit = "yr", 
                      inverse_time_unit = "1/yr",
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
  var_name = c("t_life", "R_alpha", "R_omega",
               "eta_engr_units", "eta", "p_s", "q_dot_s", 
               "p_E", "E_dot_s",
               "E_dot_emb", "C_dot_s", "C_dot_cap",
               "C_dot_om", "C_d", "C_dot_d", "C_dot_omd", 
               "C_dot_o", "N_dot", "M_dot"), 
  latex_var_name = c("$t_{li\\!f\\!e}$", "$R_\\alpha$", "$R_\\omega$",
                     "$\\eta$", "$\\eta$", "$p_s$", "$\\dot{q}_s$", 
                     "$p_E$", "$\\dot{E}_s$",
                     "$\\dot{E}_{emb}$", "$\\dot{C}_s$", "$\\dot{C}_{cap}$",
                     "$\\dot{C}_{O\\!M}$", "$C_d$", "$\\dot{C}_d$", "$\\dot{C}_{O\\!M\\!d}$", 
                     "$\\dot{C}_o$", "$\\dot{N}$", "$\\dot{M}$")
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
                       tilde = "tilde") # After macro effect
usethis::use_data(rebound_stages, overwrite = TRUE)


#
# Names of stages in LaTeX format
# 

latex_rebound_stages <- data.frame(
  stage = ReboundTools::rebound_stages %>% unlist() %>% unname(),
  latex_stage_name = c("Original ($\\circ$)", 
                       "After empl ($*$)", 
                       "After sub ($\\wedge$)",
                       "After inc ($-$)",
                       "After macro ($\\sim$)"))
usethis::use_data(latex_rebound_stages, overwrite = TRUE)


#
# Names of segments in path data frames.
# Also gives sequential order in the path graphs.
# 

rebound_segments <- list(dempl = "dempl",
                         emb = "emb",
                         cap = "cap",
                         omd = "OMd", 
                         isub = "isub", 
                         dsub = "dsub",
                         dinc = "dinc", 
                         iinc = "iinc", 
                         macro = "macro")
usethis::use_data(rebound_segments, overwrite = TRUE)

#
# Delta variables
# 

Delta_vars <- expand.grid(key_analysis_vars, rebound_stages) %>% 
  magrittr::set_names(c("key_analysis_vars", "rebound_stages")) %>% 
  dplyr::mutate(
    Delta_vars = paste0("Delta_", .data[["key_analysis_vars"]], "_", .data[["rebound_stages"]])
  ) %>% 
  dplyr::select("Delta_vars") %>% 
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
                        r = "r",
                        service_unit = "service_unit",
                        energy_engr_unit = "energy_engr_unit",
                        MJ_engr_unit = "MJ/energy_engr_unit",
                        I_E = "I_E", 
                        k = "k", 
                        p_E_engr_units = "p_E_engr_units",
                        e_qs_ps_UC_orig = "e_qs_ps_UC_orig", 
                        e_qs_M = "e_qs_M", 
                        e_qo_M = "e_qo_M")
usethis::use_data(eeu_base_params, overwrite = TRUE)


#
# Names of calculated variables at the "orig" stage.
# 

orig_vars <- list(R_alpha_orig = "R_alpha_orig", 
                  R_omega_orig = "R_omega_orig",
                  p_E = "p_E",
                  p_E_orig = "p_E_orig",
                  q_dot_s_orig = "q_dot_s_orig",
                  M_dot_orig = "M_dot_orig", 
                  C_cap_orig = "C_cap_orig", 
                  eta_engr_units_orig = "eta_engr_units_orig", 
                  eta_orig = "eta_orig", 
                  E_dot_s_orig = "E_dot_s_orig",
                  C_dot_cap_orig = "C_dot_cap_orig",
                  R_alpha_C_dot_cap_orig = "R_alpha_C_dot_cap_orig",
                  p_s_orig = "p_s_orig", 
                  C_dot_s_orig = "C_dot_s_orig",
                  C_dot_om_orig = "C_dot_om_orig", 
                  C_d_orig = "C_d_orig", 
                  C_dot_d_orig = "C_dot_d_orig",
                  R_omega_C_dot_d_orig = "R_omega_C_dot_d_orig",
                  C_dot_omd_orig = "C_dot_omd_orig",
                  C_dot_o_orig = "C_dot_o_orig",
                  f_Cs_orig = "f_Cs_orig",
                  e_qs_ps_C_orig  = "e_qs_ps_C_orig",
                  e_qs_ps_UC_orig = "e_qs_ps_UC_orig",
                  e_qo_ps_C_orig  = "e_qo_ps_C_orig", 
                  e_qo_ps_UC_orig = "e_qo_ps_UC_orig", 
                  sigma = "sigma",
                  rho = "rho",
                  E_emb_orig = "E_emb_orig",
                  t_life_orig = "t_life_orig",
                  E_dot_emb_orig = "E_dot_emb_orig", 
                  N_dot_orig = "N_dot_orig")
usethis::use_data(orig_vars, overwrite = TRUE)


#
# Names of calculated variables at the "star" stage.
# 

star_vars <- list(R_alpha_star = "R_alpha_star", 
                  R_omega_star = "R_omega_star",
                  C_cap_star	= "C_cap_star", 
                  C_dot_om_star = "C_dot_om_star", 
                  C_d_star = "C_d_star",
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
                  R_alpha_C_dot_cap_star = "R_alpha_C_dot_cap_star",
                  E_dot_emb_star = "E_dot_emb_star",
                  C_dot_s_star = "C_dot_s_star", 
                  M_dot_star = "M_dot_star",
                  N_dot_star = "N_dot_star",
                  C_dot_d_star = "C_dot_d_star", 
                  R_omega_C_dot_d_star = "R_omega_C_dot_d_star",
                  C_dot_omd_star = "C_dot_omd_star",
                  C_dot_o_star = "C_dot_o_star", 
                  f_Cs_star = "f_Cs_star",
                  e_qs_ps_C_star  = "e_qs_ps_C_star",
                  e_qs_ps_UC_star = "e_qs_ps_UC_star",
                  e_qo_ps_C_star  = "e_qo_ps_C_star", 
                  e_qo_ps_UC_star = "e_qo_ps_UC_star", 
                  p_E_star = "p_E_star",
                  E_dot_s_star = "E_dot_s_star")
usethis::use_data(star_vars, overwrite = TRUE)


#
# Names of calculated variables at the "hat" stage.
# 

hat_vars <- list(t_life_hat = "t_life_hat", 
                 R_alpha_hat = "R_alpha_hat", 
                 R_omega_hat = "R_omega_hat",
                 eta_engr_units_hat = "eta_engr_units_hat", 
                 eta_hat = "eta_hat",
                 p_s_hat = "p_s_hat",
                 C_cap_hat = "C_cap_hat", 
                 C_dot_cap_hat = "C_dot_cap_hat",
                 R_alpha_C_dot_cap_hat = "R_alpha_C_dot_cap_hat",
                 E_dot_emb_hat = "E_dot_emb_hat",
                 M_dot_hat = "M_dot_hat",
                 q_dot_s_hat = "q_dot_s_hat",
                 p_E_hat = "p_E_hat",
                 E_dot_s_hat = "E_dot_s_hat",
                 C_dot_s_hat = "C_dot_s_hat",
                 C_dot_om_hat = "C_dot_om_hat",
                 C_d_hat = "C_d_hat",
                 C_dot_d_hat = "C_dot_d_hat", 
                 R_omega_C_dot_d_hat = "R_omega_C_dot_d_hat",
                 C_dot_omd_hat = "C_dot_omd_hat",
                 C_dot_o_hat = "C_dot_o_hat", 
                 f_Cs_hat = "f_Cs_hat",
                 e_qs_ps_C_hat  = "e_qs_ps_C_hat",
                 e_qs_ps_UC_hat = "e_qs_ps_UC_hat",
                 e_qo_ps_C_hat  = "e_qo_ps_C_hat", 
                 e_qo_ps_UC_hat = "e_qo_ps_UC_hat", 
                 N_dot_hat = "N_dot_hat",
                 M_dot_hat_prime = "M_dot_hat_prime")
usethis::use_data(hat_vars, overwrite = TRUE)


#
# Names of calculated variables at the "bar" stage.
# 

bar_vars <- list(t_life_bar = "t_life_bar", 
                 R_alpha_bar = "R_alpha_bar", 
                 R_omega_bar = "R_omega_bar",
                 eta_engr_units_bar = "eta_engr_units_bar",
                 eta_bar = "eta_bar",
                 p_s_bar = "p_s_bar",
                 C_dot_cap_bar = "C_dot_cap_bar",
                 R_alpha_C_dot_cap_bar = "R_alpha_C_dot_cap_bar",
                 E_dot_emb_bar = "E_dot_emb_bar",
                 M_dot_bar = "M_dot_bar",
                 q_dot_s_bar = "q_dot_s_bar",
                 p_E_bar = "p_E_bar",
                 E_dot_s_bar = "E_dot_s_bar",
                 C_dot_s_bar = "C_dot_s_bar",
                 C_dot_om_bar = "C_dot_om_bar",
                 C_d_bar = "C_d_bar",
                 C_dot_d_bar = "C_dot_d_bar", 
                 R_omega_C_dot_d_bar = "R_omega_C_dot_d_bar",
                 C_dot_omd_bar = "C_dot_omd_bar",
                 C_dot_o_bar = "C_dot_o_bar", 
                 f_Cs_bar = "f_Cs_bar",
                 e_qs_ps_C_bar  = "e_qs_ps_C_bar",
                 e_qs_ps_UC_bar = "e_qs_ps_UC_bar",
                 e_qo_ps_C_bar  = "e_qo_ps_C_bar", 
                 e_qo_ps_UC_bar = "e_qo_ps_UC_bar", 
                 N_dot_bar = "N_dot_bar")
usethis::use_data(bar_vars, overwrite = TRUE)


#
# Names of calculated variables at the "tilde" stage.
# 

tilde_vars <- list(t_life_tilde = "t_life_tilde", 
                   R_alpha_tilde = "R_alpha_tilde", 
                   R_omega_tilde = "R_omega_tilde",
                   eta_engr_units_tilde = "eta_engr_units_tilde",
                   eta_tilde = "eta_tilde",
                   p_s_tilde = "p_s_tilde",
                   C_dot_cap_tilde = "C_dot_cap_tilde",
                   R_alpha_C_dot_cap_tilde = "R_alpha_C_dot_cap_tilde",
                   E_dot_emb_tilde = "E_dot_emb_tilde",
                   C_dot_om_tilde = "C_dot_om_tilde",
                   C_d_tilde = "C_d_tilde",
                   C_dot_d_tilde = "C_dot_d_tilde", 
                   R_omega_C_dot_d_tilde = "R_omega_C_dot_d_tilde",
                   C_dot_omd_tilde = "C_dot_omd_tilde",
                   M_dot_tilde = "M_dot_tilde",
                   q_dot_s_tilde = "q_dot_s_tilde",
                   p_E_tilde = "p_E_tilde",
                   E_dot_s_tilde = "E_dot_s_tilde",
                   C_dot_s_tilde = "C_dot_s_tilde",
                   C_dot_o_tilde = "C_dot_o_tilde", 
                   f_Cs_tilde = "f_Cs_tilde",
                   e_qs_ps_C_tilde  = "e_qs_ps_C_tilde",
                   e_qs_ps_UC_tilde = "e_qs_ps_UC_tilde",
                   e_qo_ps_C_tilde  = "e_qo_ps_C_tilde", 
                   e_qo_ps_UC_tilde = "e_qo_ps_UC_tilde", 
                   N_dot_tilde = "N_dot_tilde")
usethis::use_data(tilde_vars, overwrite = TRUE)


#
# Names of rebound terms calculated after all Delta variables are calculated.
# 

rebound_terms <- list(Re_dempl = "Re_dempl",
                      Re_emb = "Re_emb",
                      Re_cap = "Re_cap",
                      Re_om = "Re_om", 
                      Re_d = "Re_d",
                      Re_omd = "Re_omd",
                      Re_empl = "Re_empl",
                      Re_dsub = "Re_dsub", 
                      Re_isub = "Re_isub",
                      Re_sub = "Re_sub",
                      Re_dinc = "Re_dinc", 
                      Re_iinc = "Re_iinc", 
                      Re_inc = "Re_inc",
                      Re_micro = "Re_micro", 
                      Re_macro = "Re_macro", 
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
                          rebound_terms$Re_dir, 
                          rebound_terms$Re_indir, 
                          rebound_terms$Re_tot)
usethis::use_data(rebound_terms_agg, overwrite = TRUE)



#
# LaTeX names of rebound terms.
# 

latex_rebound_terms <- list(Re_dempl = "$Re_{dempl}$",
                            Re_emb = "$Re_{emb}$",
                            Re_cap = "$Re_{cap}$",
                            Re_om = "$Re_{O\\!M}$",
                            Re_d = "$Re_d$", 
                            Re_omd = "$Re_{O\\!M\\!d}$",
                            Re_empl = "$Re_{empl}$",
                            Re_dsub = "$Re_{dsub}$", 
                            Re_isub = "$Re_{isub}$",
                            Re_sub = "$Re_{sub}$",
                            Re_dinc = "$Re_{dinc}$", 
                            Re_iinc = "$Re_{iinc}$", 
                            Re_inc = "$Re_{inc}$",
                            Re_micro = "$Re_{micro}$",
                            Re_macro = "$Re_{macro}$", 
                            Re_dir = "$Re_{dir}$",
                            Re_indir = "$Re_{indir}$",
                            Re_tot = "$Re_{tot}$")
usethis::use_data(latex_rebound_terms, overwrite = TRUE)


#
# Graph types
# 

graph_types <- list(energy = "Energy",
                    expenditure = "Expenditure",
                    consumption = "Consumption")
usethis::use_data(graph_types, overwrite = TRUE)


#
# Graph data frame column names
# 

graph_df_colnames <- list(colour_col = "colour", 
                          size_col = "size", 
                          shape_col = "shape",
                          fill_col = "fill",
                          stroke_col = "stroke",
                          linewidth_col = "linewidth",
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
# macro_colour <- rgb(191, 192, 250, maxColorValue = 255, alpha = 255)

# These colours approximately match the colours in the rebound paper,
# but they are a little bolder.
# empl_colour <- "pink2"
# sub_colour <- "peachpuff"
# inc_colour <- "palegreen2"
# macro_colour <- "slateblue1"


# These colours are obtained from the viridis colour scale

v_colours <- viridis::viridis(4, option = "plasma", begin = 0.01, end = 0.95)
# v_colours <- viridis::viridis(4, direction = -1)
# v_colours <- viridis::viridis(4, direction = -1, end = 0.93)
# v_colours <- viridis::viridis(4, direction = -1, end = 0.85)
empl_colour <- v_colours[1]
sub_colour <- v_colours[2]
inc_colour <- v_colours[3]
micro_colour <- v_colours[4]
macro_colour <- v_colours[4]
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
                          
                          # Whether to show indifference curves on consumption path graphs
                          show_indifference_curves = TRUE,
                          
                          # Path colours
                          dempl_colour = empl_colour, 
                          emb_colour = empl_colour,
                          cap_colour = empl_colour,
                          omd_colour = empl_colour, 
                          empl_colour = empl_colour,
                          isub_colour = sub_colour, 
                          dsub_colour = sub_colour,
                          sub_colour = sub_colour, 
                          dinc_colour = inc_colour,
                          iinc_colour = inc_colour, 
                          inc_colour = inc_colour,
                          micro_colour = micro_colour,
                          macro_colour = macro_colour, 
                          dir_colour = tot_colour, 
                          indir_colour = tot_colour, 
                          tot_colour = tot_colour,
                          
                          # Path line widths
                          dempl_linewidth = 1, 
                          emb_linewidth = 1.5,
                          cap_linewidth = 1.5,
                          omd_linewidth = 1, 
                          empl_linewidth = 1,
                          isub_linewidth = 1,
                          dsub_linewidth = 1,
                          sub_linewidth = 1,
                          dinc_linewidth = 1,
                          iinc_linewidth = 1, 
                          inc_linewidth = 1,
                          micro_linewidth = 1,
                          macro_linewidth = 1,
                          dir_linewidth = 1,
                          indir_linewidth = 1,
                          tot_linewidth = 2,
                          
                          # Path linetypes
                          dempl_linetype = "solid",
                          emb_linetype = "11",
                          cap_linetype = "11",
                          omd_linetype = "solid", 
                          empl_linetype = "solid",
                          dsub_linetype = "solid",
                          isub_linetype = "solid", 
                          sub_linetype = "solid",
                          dinc_linetype = "solid",
                          iinc_linetype = "solid", 
                          sinc_linetype = "solid",
                          micro_linetype = "solid",
                          macro_linetype = "solid",
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
                          expenditure_grid_colour = "black",
                          cons_grid_colour = "black",
                          cons_ray_colour = "black",
                          cons_indiff_grid_colour = "black",
                          
                          # Grid line sizes
                          energy_grid_linewidth = 0.1,
                          zero_perc_rebound_grid_linewidth = 0.3,
                          hundred_perc_rebound_grid_linewidth = 0.3,
                          energy_rebound_lines_linewidth = 0.1,
                          expenditure_grid_linewidth = 0.3,
                          cons_grid_linewidth = 0.1,
                          cons_ray_linewidth = 0.1,
                          cons_indiff_grid_linewidth = 0.5,
                          
                          # Grid line types
                          energy_grid_linetype = "solid",
                          zero_perc_rebound_grid_linetype = "solid",
                          hundred_perc_rebound_grid_linetype = "solid",
                          energy_rebound_lines_linetype = "solid",
                          expenditure_grid_linetype = "solid",
                          cons_grid_linetype = "solid",
                          cons_ray_linetype = "solid",
                          cons_indiff_grid_linetype = "solid", 
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
                          omd_colour = empl_colour, 
                          empl_colour = empl_colour, 
                          dsub_colour = sub_colour,
                          isub_colour = sub_colour, 
                          sub_colour = sub_colour, 
                          dinc_colour = inc_colour,
                          iinc_colour = inc_colour, 
                          inc_colour = inc_colour, 
                          micro_colour = micro_colour, 
                          macro_colour = macro_colour, 
                          dir_colour = tot_colour,
                          indir_colour = tot_colour,
                          tot_colour = tot_colour,
                          
                          # Line widths for rebound effects
                          dempl_linewidth = 0.5, 
                          emb_linewidth = 0.5,
                          cap_linewidth = 0.5,
                          omd_linewidth = 0.5, 
                          empl_linewidth = 0.5,
                          dsub_linewidth = 0.5,
                          isub_linewidth = 0.5,
                          sub_linewidth = 0.5,
                          dinc_linewidth = 0.5,
                          iinc_linewidth = 0.5,
                          inc_linewidth = 0.5,
                          micro_linewidth = 0.5,
                          macro_linewidth = 0.5,
                          dir_linewidth = 0.5,
                          indir_linewidth = 0.5,
                          tot_linewidth = 1,
                          
                          # linetypes for rebound effects
                          dempl_linetype = "solid",
                          emb_linetype = "22",
                          cap_linetype = "22",
                          omd_linetype = "41", 
                          empl_linetype = "solid",
                          dsub_linetype = "solid",
                          isub_linetype = "22",
                          sub_linetype = "solid",
                          dinc_linetype = "solid",
                          iinc_linetype = "22", 
                          inc_linetype = "solid",
                          micro_linetype = "solid",
                          macro_linetype = "solid",
                          dir_linetype = "solid", 
                          indir_linetype = "22", 
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
