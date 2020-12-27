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
# Names of base parameters for a rebound analysis.
# 

eeu_base_params <- list(case = "Case", 
                      original = "Original", 
                      upgrade = "Upgrade", 
                      MJ_engr_unit = "MJ/engr_unit",
                      I_E = "I_E", 
                      k = "k", 
                      p_E = "p_E",
                      e_qs_ps_UC = "e_qs_ps_UC", 
                      e_qs_M = "e_qs_M", 
                      e_qo_M = "e_qo_M")
usethis::use_data(eeu_base_params, overwrite = TRUE)


#
# Names of calculated variables at the "orig" stage.
# 

orig_vars <- list(q_dot_s_orig = "q_dot_s_orig",
                  M_dot_orig = "M_dot_orig", 
                  C_cap_orig = "C_cap_orig", 
                  t_orig = "t_orig", 
                  C_dot_md_orig = "C_dot_md_orig", 
                  eta_orig_engr_units = "eta_orig_engr_units", 
                  eta_orig = "eta_orig", 
                  E_dot_s_orig = "E_dot_s_orig",
                  C_dot_cap_orig = "C_dot_cap_orig",
                  p_s_orig = "p_s_orig", 
                  C_dot_s_orig = "C_dot_s_orig",
                  C_dot_o_orig = "C_dot_o_orig",
                  f_Cs_orig = "f_Cs_orig",
                  e_qs_ps = "e_qs_ps",
                  e_qo_ps = "e_qo_ps", 
                  E_emb_orig = "E_emb_orig",
                  E_dot_emb_orig = "E_dot_emb_orig", 
                  N_dot_orig = "N_dot_orig")
usethis::use_data(orig_vars, overwrite = TRUE)


#
# Names of calculated variables at the "star" stage.
# 

star_vars <- list(C_cap_star	= "C_cap_star", 
                  t_star = "t_star", 
                  C_dot_md_star = "C_dot_md_star", 
                  E_emb_star = "E_emb_star",
                  eta_star_engr_units = "eta_star_engr_units", 
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

hat_vars <- list(eta_hat = "eta_hat",
                 p_s_hat = "p_s_hat",
                 C_dot_cap_hat = "C_dot_cap_hat",
                 C_dot_md_hat = "C_dot_md_hat",
                 E_dot_emb_hat = "E_dot_emb_hat",
                 q_dot_s_hat = "q_dot_s_hat", 
                 C_dot_o_hat = "C_dot_o_hat")
usethis::use_data(hat_vars, overwrite = TRUE)



