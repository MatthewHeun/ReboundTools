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
                      I_E = "I_E", 
                      k = "k", 
                      p_E = "p_E",
                      eta_orig = "eta_orig", 
                      eta_tilde = "eta_tilde", 
                      eps_ps_qs_UC = "eps_ps_qs_UC", 
                      eps_qs_M = "eps_qs_M", 
                      qps_po_M = "eps_qo_M", 
                      q_dot_s_orig = "q_dot_s_orig",
                      M_dot_orig = "M_dot_orig", 
                      C_cap_orig = "C_cap_orig", 
                      R_orig = "R_orig", 
                      t_orig = "t_orig", 
                      C_cap_star	= "C_cap_star", 
                      R_star = "R_star", 
                      t_star = "t_star", 
                      C_dot_md_orig = "C_dot_md_orig", 
                      C_dot_md_star = "C_dot_md_star")
usethis::use_data(eeu_base_data, overwrite = TRUE)
