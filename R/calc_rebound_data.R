


#' Calculate energy rebound data at the original stage
#' 
#' This function calculates energy rebound information for the original 
#' stage (pre-EEU).
#'
#' @param .rebound_data An optional data frame containing EEU base data. 
#'                      See `ReboundTools::eeu_base_params`.
#' @param eta_orig_engr_units,MJ_engr_unit,q_dot_s_orig,C_cap_orig,t_orig,p_E,M_dot_orig,C_dot_md_orig,e_qs_ps_UC,e_qs_m,E_emb_orig See `ReboundTools::orig_vars`.
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
                      eta_orig_engr_units = ReboundTools::eeu_base_params$eta_orig_engr_units,
                      MJ_engr_unit = ReboundTools::eeu_base_params$MJ_engr_unit,
                      q_dot_s_orig = ReboundTools::eeu_base_params$q_dot_s_orig,
                      C_cap_orig = ReboundTools::eeu_base_params$C_cap_orig, 
                      t_orig = ReboundTools::eeu_base_params$t_orig,
                      p_E = ReboundTools::eeu_base_params$p_E,
                      M_dot_orig = ReboundTools::eeu_base_params$M_dot_orig,
                      C_dot_md_orig = ReboundTools::eeu_base_params$C_dot_md_orig,
                      e_qs_ps_UC = ReboundTools::eeu_base_params$e_qs_ps_UC,
                      e_qs_M = ReboundTools::eeu_base_params$e_qs_M,
                      E_emb_orig = ReboundTools::eeu_base_params$E_emb_orig,
                      # Output names
                      eta_orig = ReboundTools::orig_vars$eta_orig,
                      E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
                      C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig,
                      p_s_orig = ReboundTools::orig_vars$p_s_orig,
                      C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig,
                      C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                      f_Cs_orig = ReboundTools::orig_vars$f_Cs_orig,
                      e_qs_ps = ReboundTools::orig_vars$e_qs_ps,
                      e_qo_ps = ReboundTools::orig_vars$e_qo_ps,
                      E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig,
                      N_dot_orig = ReboundTools::orig_vars$N_dot_orig) {
  
  calc_orig_fun <- function(MJ_engr_unit_val,
                            eta_orig_engr_units_val,
                            q_dot_s_orig_val,
                            C_cap_orig_val,
                            t_orig_val,
                            p_E_val,
                            M_dot_orig_val,
                            C_dot_md_orig_val,
                            e_qs_ps_UC_val,
                            e_qs_M_val,
                            E_emb_orig_val) {
    
    eta_orig_val <- eta_orig_engr_units_val / MJ_engr_unit_val
    E_dot_s_orig_val <- q_dot_s_orig_val / eta_orig_val
    C_dot_cap_orig_val <- C_cap_orig_val / t_orig_val
    p_s_orig_val <- p_E_val / eta_orig_val
    C_dot_s_orig_val <- p_E_val * E_dot_s_orig_val
    C_dot_o_orig_val <- M_dot_orig_val - C_dot_s_orig_val - C_dot_cap_orig_val - C_dot_md_orig_val
    f_Cs_orig_val <- C_dot_s_orig_val / (C_dot_s_orig_val + C_dot_o_orig_val)
    e_qs_ps_val <- e_qs_ps_UC_val + f_Cs_orig_val*e_qs_M_val
    e_qo_ps_val <- f_Cs_orig_val*(f_Cs_orig_val + e_qs_ps_UC_val) / (f_Cs_orig_val - 1)
    E_dot_emb_orig_val <- E_emb_orig_val / t_orig_val
    N_dot_orig_val <- 0
    
    list(eta_orig_val,
         E_dot_s_orig_val,
         C_dot_cap_orig_val,
         p_s_orig_val,
         C_dot_s_orig_val,
         C_dot_o_orig_val,
         f_Cs_orig_val,
         e_qs_ps_val,
         e_qo_ps_val, 
         E_dot_emb_orig_val,
         N_dot_orig_val) %>% 
      magrittr::set_names(c(eta_orig,
                            E_dot_s_orig,
                            C_dot_cap_orig,
                            p_s_orig,
                            C_dot_s_orig,
                            C_dot_o_orig,
                            f_Cs_orig,
                            e_qs_ps, 
                            e_qo_ps, 
                            E_dot_emb_orig,
                            N_dot_orig))
  }
  
  matsindf::matsindf_apply(.eeu_data, FUN = calc_orig_fun, 
                           MJ_engr_unit_val = MJ_engr_unit,
                           q_dot_s_orig_val = q_dot_s_orig,
                           eta_orig_engr_units_val = eta_orig_engr_units,
                           C_cap_orig_val = C_cap_orig,
                           t_orig_val = t_orig,
                           p_E_val = p_E,
                           M_dot_orig_val = M_dot_orig,
                           C_dot_md_orig_val = C_dot_md_orig,
                           e_qs_ps_UC_val = e_qs_ps_UC,
                           e_qs_M_val = e_qs_M,
                           E_emb_orig_val = E_emb_orig)
}


#' Calculate energy rebound data at the star stage
#' 
#' This function calculates energy rebound information for the star 
#' stage (immediately after the emplacement effect, with no behavior change).
#'
#' @param .orig_data An optional data frame containing EEU base data and original data, 
#'                   likely calculated by `calc_orig()`.
#'                   See `ReboundTools::eeu_base_params` and `ReboundTools::orig_vars`.
#'
#' @return A list or data frame of derived rebound values for the star stage (after the emplacement effect).
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   calc_orig() %>% 
#'   calc_star()
calc_star <- function(.orig_data = NULL,
                      # Input names
                      eta_star_engr_units = ReboundTools::eeu_base_params$eta_star_engr_units,
                      MJ_engr_unit = ReboundTools::eeu_base_params$MJ_engr_unit,
                      eta_orig = ReboundTools::orig_vars$eta_orig,
                      E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
                      p_E = ReboundTools::eeu_base_params$p_E,
                      q_dot_s_orig = ReboundTools::eeu_base_params$q_dot_s_orig,
                      C_cap_star = ReboundTools::eeu_base_params$C_cap_star,
                      t_star = ReboundTools::eeu_base_params$t_star,
                      E_emb_star = ReboundTools::eeu_base_params$E_emb_star,
                      M_dot_orig = ReboundTools::eeu_base_params$M_dot_orig,
                      C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig,
                      C_dot_md_orig = ReboundTools::eeu_base_params$C_dot_md_orig,
                      C_dot_md_star = ReboundTools::eeu_base_params$C_dot_md_star,
                      C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                      
                      # Output names
                      eta_star = ReboundTools::star_vars$eta_star,
                      eta_ratio = ReboundTools::star_vars$eta_ratio,
                      S_dot_dev = ReboundTools::star_vars$S_dot_dev,
                      G_dot = ReboundTools::star_vars$G_dot,
                      p_s_star = ReboundTools::star_vars$p_s_star,
                      q_dot_s_star = ReboundTools::star_vars$q_dot_s_star,
                      C_dot_cap_star = ReboundTools::star_vars$C_dot_cap_star,
                      E_dot_emb_star = ReboundTools::star_vars$E_dot_emb_star,
                      C_dot_s_star = ReboundTools::star_vars$C_dot_s_star,
                      M_dot_star = ReboundTools::star_vars$M_dot_star, 
                      N_dot_star = ReboundTools::star_vars$N_dot_star,
                      C_dot_o_star = ReboundTools::star_vars$C_dot_o_star,
                      E_dot_s_star = ReboundTools::star_vars$E_dot_s_star) {
  
  
  calc_star_fun <- function(MJ_engr_unit_val, 
                            eta_orig_val,
                            eta_star_engr_units_val,
                            E_dot_s_orig_val, 
                            p_E_val, 
                            q_dot_s_orig_val, 
                            C_cap_star_val,
                            t_star_val,
                            E_emb_star_val,
                            M_dot_orig_val, 
                            C_dot_cap_orig_val,
                            C_dot_md_orig_val,
                            C_dot_md_star_val, 
                            C_dot_o_orig_val) {
    
    eta_star_val <- eta_star_engr_units_val / MJ_engr_unit_val
    eta_ratio_val <- eta_star_val / eta_orig_val
    S_dot_dev_val <- (eta_ratio_val - 1) * (1/eta_ratio_val) * E_dot_s_orig_val
    G_dot_val <- p_E_val * S_dot_dev_val
    p_s_star_val <- p_E_val / eta_star_val
    q_dot_s_star_val <- q_dot_s_orig_val
    C_dot_cap_star_val <- C_cap_star_val / t_star_val
    E_dot_emb_star_val <- E_emb_star_val / t_star_val
    C_dot_s_star_val <- p_s_star_val * q_dot_s_star_val
    M_dot_star_val <- M_dot_orig_val
    N_dot_star_val <- G_dot_val - (C_dot_cap_star_val - C_dot_cap_orig_val) - (C_dot_md_star_val - C_dot_md_orig_val)
    C_dot_o_star_val <- C_dot_o_orig_val
    E_dot_s_star_val <- q_dot_s_star_val / eta_star_val

        
    list(eta_star_val,
         eta_ratio_val,
         S_dot_dev_val,
         G_dot_val, 
         p_s_star_val,
         q_dot_s_star_val,
         C_dot_cap_star_val,
         E_dot_emb_star_val,
         C_dot_s_star_val,
         M_dot_star_val,
         N_dot_star_val,
         C_dot_o_star_val,
         E_dot_s_star_val
    ) %>% 
      magrittr::set_names(c(eta_star,
                            eta_ratio,
                            S_dot_dev,
                            G_dot,
                            p_s_star,
                            q_dot_s_star,
                            C_dot_cap_star,
                            E_dot_emb_star,
                            C_dot_s_star,
                            M_dot_star,
                            N_dot_star,
                            C_dot_o_star, 
                            E_dot_s_star))
  }
  
  matsindf::matsindf_apply(.orig_data, FUN = calc_star_fun, 
                           MJ_engr_unit_val = MJ_engr_unit,
                           eta_orig_val = eta_orig,
                           eta_star_engr_units_val = eta_star_engr_units,
                           E_dot_s_orig_val = E_dot_s_orig,
                           p_E_val = p_E, 
                           q_dot_s_orig_val = q_dot_s_orig, 
                           C_cap_star_val = C_cap_star,
                           t_star_val = t_star,
                           E_emb_star_val = E_emb_star,
                           M_dot_orig_val = M_dot_orig,
                           C_dot_cap_orig_val = C_dot_cap_orig,
                           C_dot_md_orig_val = C_dot_md_orig,
                           C_dot_md_star_val = C_dot_md_star,
                           C_dot_o_orig_val = C_dot_o_orig)
}





#' Calculate energy rebound data at the hat stage
#' 
#' This function calculates energy rebound information for the hat
#' stage (immediately after the substitution effect).
#'
#'
#' @param .star_data An optional data frame containing EEU base data, original data, 
#'                   and star data, 
#'                   likely calculated by `calc_star()`.
#'                   
#' @return A list or data frame of derived rebound values for the hat stage (after the substitution effect).
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   calc_orig() %>% 
#'   calc_star() %>% 
#'   calc_hat()
calc_hat <- function(.star_data = NULL,
                     # Input names
                     eta_star = ReboundTools::star_vars$eta_star,
                     
                     # Output names
                     eta_hat = ReboundTools::hat_vars$eta_hat
) {
  
  calc_hat_fun <- function(eta_star_val) {
    eta_hat_val <- eta_star_val
    
    list(eta_hat_val) %>% 
      magrittr::set_names(c(eta_hat))
  }
  
  matsindf::matsindf_apply(.star_data, FUN = calc_hat_fun, 
                           eta_star_val = eta_star)
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
                      

