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
# Names of required columns in an EEU data table.
# 

eeu_base_data <- list(case = "Case", 
                      original = "Original", 
                      upgrade = "Upgrade", 
                      MJ_engr_unit = "MJ/engr_unit",
                      I_E = "I_E", 
                      k = "k", 
                      p_E = "p_E",
                      eta_orig_engr_units = "eta_orig_engr_units", 
                      eta_tilde_engr_units = "eta_tilde_engr_units", 
                      e_ps_qs_UC = "e_ps_qs_UC", 
                      e_qs_M = "e_qs_M", 
                      q_po_M = "e_qo_M", 
                      q_dot_s_orig = "q_dot_s_orig",
                      M_dot_orig = "M_dot_orig", 
                      C_cap_orig = "C_cap_orig", 
                      t_orig = "t_orig", 
                      C_cap_star	= "C_cap_star", 
                      t_star = "t_star", 
                      C_dot_md_orig = "C_dot_md_orig", 
                      C_dot_md_star = "C_dot_md_star", 
                      E_emb_orig = "E_emb_orig", 
                      E_emb_star = "E_emb_star")
usethis::use_data(eeu_base_data, overwrite = TRUE)


#
# Names of calculated variables
# 

eeu_derived_data <- list(eta_orig = "eta_orig", 
                         eta_tilde = "eta_tilde", 
                         eta_ratio = "eta_ratio", 
                         E_dot_s_orig = "E_dot_s_orig",
                         S_dot_dev = "S_dot_dev", 
                         G_dot = "G_dot")
usethis::use_data(eeu_derived_data, overwrite = TRUE)
