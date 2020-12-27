


#' Calculate energy rebound derived data
#'
#' @param .rebound_data An optional data frame containing EEU base data. See `ReboundTools::eeu_base_data`.
#' @param eta_orig,eta_tilde See `ReboundTools::eeu_base_data`.
#'
#' @return A list or data frame of derived rebound values.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   calc_orig()
calc_orig <- function(.eeu_data = NULL,
                      # Input names
                      eta_orig_engr_units = ReboundTools::eeu_base_data$eta_orig_engr_units,
                      MJ_engr_unit = ReboundTools::eeu_base_data$MJ_engr_unit,
                      q_dot_s_orig = ReboundTools::eeu_base_data$q_dot_s_orig,
                      
                      # p_E = ReboundTools::eeu_base_data$p_E,
                      # e_qs_ps_UC = ReboundTools::eeu_base_data$e_qs_ps_UC, 
                      # e_qs_M = ReboundTools::eeu_base_data$e_qs_M, 
                      # e_qo_M = ReboundTools::eeu_base_data$e_qo_M, 
                      # 
                      # M_dot_orig = ReboundTools::eeu_base_data$M_dot_orig, 
                      # C_cap_orig = ReboundTools::eeu_base_data$C_cap_orig, 
                      # t_orig = ReboundTools::eeu_base_data$t_orig, 
                      # C_dot_md_orig = ReboundTools::eeu_base_data$C_dot_md_orig, 
                      # E_emb_orig = ReboundTools::eeu_base_data$E_emb_orig, 
                      
                      # Output names
                      eta_orig = ReboundTools::eeu_derived_data$eta_orig,
                      E_dot_s_orig = ReboundTools::eeu_derived_data$E_dot_s_orig
                      # C_dot_cap_orig = ReboundTools::eeu_derived_data$C_dot_cap_orig, 
                      # C_dot_cap_star = ReboundTools::eeu_derived_data$C_dot_cap_star, 
                      # p_s_orig = ReboundTools::eeu_derived_data$p_s_orig, 
                      # C_dot_s_orig = ReboundTools::eeu_derived_data$C_dot_s_orig,
                      # C_dot_o_orig = ReboundTools::eeu_derived_data$C_dot_o_orig,
                      # f_qs_orig = ReboundTools::eeu_derived_data$f_qs_orig,
                      # e_qs_ps = ReboundTools::eeu_derived_data$e_qs_ps
                      ) {
  
  calc_orig_fun <- function(MJ_engr_unit_val,
                            eta_orig_engr_units_val,
                            q_dot_s_orig_val
                            # p_E_val,
                            # e_qs_ps_UC_val, 
                            # e_qs_M_val, 
                            # q_qo_M_val, 
                            # M_dot_orig_val, 
                            # C_cap_orig_val, 
                            # t_orig_val, 
                            # C_dot_md_orig_val, 
                            # E_emb_orig_val) {
  ){
    
    eta_orig_val <- eta_orig_engr_units_val / MJ_engr_unit_val
    E_dot_s_orig_val <- q_dot_s_orig_val / eta_orig_val
    # S_dot_dev_val <- (eta_ratio_val - 1) * (1/eta_ratio_val) * E_dot_s_orig_val
    # G_dot_val <- p_E_val * S_dot_dev_val
    # C_dot_cap_orig_val <- C_cap_orig_val / t_orig_val
    # C_dot_cap_star_val <- C_cap_star_val / t_star_val
    # p_s_orig_val <- p_E_val / eta_orig_val
    # p_s_star_val <- p_E_val / eta_tilde_val
    # # Work on elasticities
    # C_dot_s_orig_val <- p_E_val * E_dot_s_orig_val
    # C_dot_o_orig_val <- M_dot_orig_val - C_dot_s_orig_val - C_dot_cap_orig_val - C_dot_md_orig_val
    # f_qs_orig_val <- C_dot_s_orig_val / (C_dot_s_orig_val + C_dot_o_orig_val)
    # e_qs_ps_val <- e_qs_ps_UC_val + f_qs_orig_val*e_qs_M_val
    # # Work on quantities of energy service consumed (q_dot_s)
    # q_dot_s_star_val <- q_dot_s_orig_val
    # q_dot_s_hat_val <- q_dot_s_star_val * (eta_ratio_val)^e_qs_ps_val
    
    
    list(eta_orig_val,
         E_dot_s_orig_val
         # S_dot_dev_val, 
         # G_dot_val, 
         # C_dot_cap_orig_val, 
         # C_dot_cap_star_val, 
         # p_s_orig_val, 
         # p_s_star_val, 
         # C_dot_s_orig_val,
         # C_dot_o_orig_val, 
         # f_qs_orig_val, 
         # e_qs_ps_val, 
         # q_dot_s_star_val, 
         # q_dot_s_hat_val
         ) %>% magrittr::set_names(c(eta_orig,
                                                    E_dot_s_orig
                                                    # S_dot_dev, 
                                                    # G_dot, 
                                                    # C_dot_cap_orig, 
                                                    # C_dot_cap_star, 
                                                    # p_s_orig, 
                                                    # p_s_star, 
                                                    # C_dot_s_orig, 
                                                    # C_dot_o_orig,
                                                    # f_qs_orig,
                                                    # e_qs_ps,
                                                    # q_dot_s_star, 
                                                    # q_dot_s_hat
                                     ))
  }
  
  
  matsindf::matsindf_apply(.eeu_data, FUN = calc_orig_fun, 
                           MJ_engr_unit_val = MJ_engr_unit,
                           q_dot_s_orig_val = q_dot_s_orig,
                           eta_orig_engr_units_val = eta_orig_engr_units
                           # p_E_val = p_E,
                           # e_qs_ps_UC_val = e_qs_ps_UC, 
                           # e_qs_M_val = e_qs_M, 
                           # q_qo_M_val = e_qo_M, 
                           # M_dot_orig_val = M_dot_orig, 
                           # C_cap_orig_val = C_cap_orig, 
                           # t_orig_val = t_orig, 
                           # C_dot_md_orig_val = C_dot_md_orig, 
                           # E_emb_orig_val = E_emb_orig
                           )
  
}



calc_star <- function(.orig_data = NULL,
                      # Input names
                      MJ_engr_unit = ReboundTools::eeu_base_data$MJ_engr_unit,
                      I_E = ReboundTools::eeu_base_data$I_E, 
                      k = ReboundTools::eeu_base_data$k, 
                      p_E = ReboundTools::eeu_base_data$p_E,
                      eta_orig_engr_units = ReboundTools::eeu_base_data$eta_orig_engr_units,
                      eta_tilde_engr_units = ReboundTools::eeu_base_data$eta_tilde_engr_units,
                      e_qs_ps_UC = ReboundTools::eeu_base_data$e_qs_ps_UC, 
                      e_qs_M = ReboundTools::eeu_base_data$e_qs_M, 
                      e_qo_M = ReboundTools::eeu_base_data$e_qo_M, 
                      q_dot_s_orig = ReboundTools::eeu_base_data$q_dot_s_orig,
                      M_dot_orig = ReboundTools::eeu_base_data$M_dot_orig, 
                      C_cap_orig = ReboundTools::eeu_base_data$C_cap_orig, 
                      t_orig = ReboundTools::eeu_base_data$t_orig, 
                      C_cap_star	= ReboundTools::eeu_base_data$C_cap_star, 
                      t_star = ReboundTools::eeu_base_data$t_star, 
                      C_dot_md_orig = ReboundTools::eeu_base_data$C_dot_md_orig, 
                      C_dot_md_star = ReboundTools::eeu_base_data$C_dot_md_star, 
                      E_emb_orig = ReboundTools::eeu_base_data$E_emb_orig, 
                      E_emb_star = ReboundTools::eeu_base_data$E_emb_star,
                      # Output names
                      p_s_star = ReboundTools::eeu_derived_data$p_s_star, 
                      S_dot_dev = ReboundTools::eeu_derived_data$S_dot_dev, 
                      G_dot = ReboundTools::eeu_derived_data$G_dot,
                      q_dot_s_star = ReboundTools::eeu_derived_data$q_dot_s_star) {
  
}



calc_hat <- function(.star_data = NULL
                     # Input names
                     # 
                     # 
                     # Output names
) {
  eta_tilde_val <- eta_tilde_engr_units_val / MJ_engr_unit_val
  
}



calc_bar <- function(.hat_data = NULL
                       # Input names
                       # 
                       # 
                       # Output names
) {
  
}



calc_tilde <- function(.bar_data = NULL
                     # Input names
                     # 
                     # 
                     # Output names
) {
  
  
  

  
}
                      

