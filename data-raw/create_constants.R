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
# Names of key rebound variables
# 

rebound_vars <- c("eta", "p_s", "q_dot_s", "E_dot_s", "E_dot_emb", 
                  "C_dot_s", "C_dot_cap", "C_dot_md", "C_dot_o", "M_dot", "N_dot")
usethis::use_data(rebound_vars, overwrite = TRUE)


#
# Delta variables
# 

Delta_vars <- expand.grid(rebound_vars, rebound_stages) %>% 
  magrittr::set_names(c("rebound_vars", "rebound_stages")) %>% 
  dplyr::mutate(
    Delta_vars = paste0("∆", .data[["rebound_vars"]], "_", .data[["rebound_stages"]])
  ) %>% 
  dplyr::select(.data[["Delta_vars"]]) %>% 
  unlist()
Delta_vars <- Delta_vars %>% 
  magrittr::set_names(Delta_vars) %>% 
  as.list()
usethis::use_data(Delta_vars, overwrite = TRUE)


#
# Names of stages
# 

rebound_stages <- c("orig", "star", "hat", "bar", "tilde")
usethis::use_data(rebound_stages, overwrite = TRUE)


#
# Names of base parameters for a rebound analysis.
# 

eeu_base_params <- list(reference = "Reference",
                        case = "Case", 
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
                  t_own_orig = "t_own_orig", 
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

bar_vars <- list(eta_bar = "eta_bar",
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

tilde_vars <- list(eta_tilde = "eta_tilde",
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

