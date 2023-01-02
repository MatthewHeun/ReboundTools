


#' Calculate energy rebound data at the original stage
#' 
#' This function calculates energy rebound information for the original 
#' stage (pre-EEU).
#'
#' @param .eeu_data An optional data frame containing EEU base data. 
#'                  See `ReboundTools::eeu_base_params`.
#' @param MJ_engr_unit,p_E_engr_units,e_qs_ps_UC_orig,e_qs_M,e_qo_M See `ReboundTools::eeu_base_params`.
#' @param eta_engr_units_orig,q_dot_s_orig,C_cap_orig,t_own_orig,M_dot_orig,C_dot_md_orig,E_emb_orig,t_life_orig,p_E,eta_orig,E_dot_s_orig,C_dot_cap_orig,p_s_orig,C_dot_s_orig,C_dot_o_orig,f_Cs_orig,e_qo_ps_UC_orig,e_qs_ps_C_orig,e_qo_ps_C_orig,sigma,rho,E_dot_emb_orig,N_dot_orig See `ReboundTools::orig_vars`.
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
                      MJ_engr_unit = ReboundTools::eeu_base_params$MJ_engr_unit,
                      p_E_engr_units = ReboundTools::eeu_base_params$p_E_engr_units,
                      e_qs_ps_UC_orig = ReboundTools::eeu_base_params$e_qs_ps_UC_orig,
                      e_qs_M = ReboundTools::eeu_base_params$e_qs_M,
                      e_qo_M = ReboundTools::eeu_base_params$e_qo_M,
                      
                      eta_engr_units_orig = ReboundTools::orig_vars$eta_engr_units_orig,
                      q_dot_s_orig = ReboundTools::orig_vars$q_dot_s_orig,
                      C_cap_orig = ReboundTools::orig_vars$C_cap_orig, 
                      t_own_orig = ReboundTools::orig_vars$t_own_orig,
                      M_dot_orig = ReboundTools::orig_vars$M_dot_orig,
                      C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                      E_emb_orig = ReboundTools::orig_vars$E_emb_orig,
                      t_life_orig = ReboundTools::orig_vars$t_life_orig,

                      # Output names
                      p_E = ReboundTools::orig_vars$p_E,
                      eta_orig = ReboundTools::orig_vars$eta_orig,
                      E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
                      C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig,
                      p_s_orig = ReboundTools::orig_vars$p_s_orig,
                      C_dot_s_orig = ReboundTools::orig_vars$C_dot_s_orig,
                      C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                      f_Cs_orig = ReboundTools::orig_vars$f_Cs_orig,
                      e_qo_ps_UC_orig = ReboundTools::orig_vars$e_qo_ps_UC_orig,
                      e_qs_ps_C_orig = ReboundTools::orig_vars$e_qs_ps_C_orig,
                      e_qo_ps_C_orig = ReboundTools::orig_vars$e_qo_ps_C_orig,
                      sigma = ReboundTools::orig_vars$sigma,
                      rho = ReboundTools::orig_vars$rho,
                      E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig,
                      N_dot_orig = ReboundTools::orig_vars$N_dot_orig) {
  
  calc_orig_fun <- function(MJ_engr_unit_val,
                            eta_engr_units_orig_val,
                            q_dot_s_orig_val,
                            C_cap_orig_val,
                            t_own_orig_val,
                            p_E_engr_units_val,
                            M_dot_orig_val,
                            C_dot_md_orig_val,
                            e_qs_ps_UC_orig_val,
                            e_qs_M_val,
                            e_qo_M_val,
                            sigma_val,
                            E_emb_orig_val,
                            t_life_orig_val) {
    
    p_E_val <- p_E_engr_units_val / MJ_engr_unit_val
    eta_orig_val <- eta_engr_units_orig_val / MJ_engr_unit_val
    E_dot_s_orig_val <- q_dot_s_orig_val / eta_orig_val
    C_dot_cap_orig_val <- C_cap_orig_val / t_own_orig_val
    p_s_orig_val <- p_E_val / eta_orig_val
    C_dot_s_orig_val <- p_E_val * E_dot_s_orig_val
    C_dot_o_orig_val <- M_dot_orig_val - C_dot_s_orig_val - C_dot_cap_orig_val - C_dot_md_orig_val
    f_Cs_orig_val <- C_dot_s_orig_val / (C_dot_s_orig_val + C_dot_o_orig_val)
    sigma_val <- (f_Cs_orig_val + e_qs_ps_UC_orig_val) / (f_Cs_orig_val - 1)
    rho_val <- (sigma_val - 1)/sigma_val
    e_qo_ps_UC_orig_val <- f_Cs_orig_val * (sigma_val - e_qo_M_val)
    e_qs_ps_C_orig_val <- e_qs_ps_UC_orig_val + f_Cs_orig_val*e_qs_M_val
    e_qo_ps_C_orig_val <- f_Cs_orig_val*(f_Cs_orig_val + e_qs_ps_UC_orig_val) / (f_Cs_orig_val - 1)
    E_dot_emb_orig_val <- E_emb_orig_val / t_life_orig_val
    N_dot_orig_val <- 0
    
    list(p_E_val,
         eta_orig_val,
         E_dot_s_orig_val,
         C_dot_cap_orig_val,
         p_s_orig_val,
         C_dot_s_orig_val,
         C_dot_o_orig_val,
         f_Cs_orig_val,
         e_qo_ps_UC_orig_val,
         e_qs_ps_C_orig_val,
         e_qo_ps_C_orig_val, 
         sigma_val,
         rho_val,
         E_dot_emb_orig_val,
         N_dot_orig_val) %>% 
      magrittr::set_names(c(p_E, 
                            eta_orig,
                            E_dot_s_orig,
                            C_dot_cap_orig,
                            p_s_orig,
                            C_dot_s_orig,
                            C_dot_o_orig,
                            f_Cs_orig,
                            e_qo_ps_UC_orig,
                            e_qs_ps_C_orig, 
                            e_qo_ps_C_orig, 
                            sigma,
                            rho,
                            E_dot_emb_orig,
                            N_dot_orig))
  }
  
  matsindf::matsindf_apply(.eeu_data, FUN = calc_orig_fun, 
                           MJ_engr_unit_val = MJ_engr_unit,
                           q_dot_s_orig_val = q_dot_s_orig,
                           eta_engr_units_orig_val = eta_engr_units_orig,
                           C_cap_orig_val = C_cap_orig,
                           t_own_orig_val = t_own_orig,
                           p_E_engr_units_val = p_E_engr_units,
                           p_E_val = p_E,
                           M_dot_orig_val = M_dot_orig,
                           C_dot_md_orig_val = C_dot_md_orig,
                           e_qs_ps_UC_orig_val = e_qs_ps_UC_orig,
                           e_qs_M_val = e_qs_M,
                           e_qo_M_val = e_qo_M,
                           sigma_val = sigma,
                           E_emb_orig_val = E_emb_orig, 
                           t_life_orig_val = t_life_orig)
}


#' Calculate energy rebound data at the star stage
#' 
#' This function calculates energy rebound information for the star 
#' stage (immediately after the emplacement effect, with no behavior change).
#'
#' @param .orig_data An optional data frame containing EEU base data and original data, 
#'                   likely calculated by `calc_orig()`.
#' @param MJ_engr_unit,p_E See `ReboundTools::eeu_base_params`.
#' @param eta_orig,E_dot_s_orig,q_dot_s_orig,M_dot_orig,C_dot_cap_orig,C_dot_md_orig,C_dot_o_orig,e_qs_ps_UC_orig,e_qo_ps_UC_orig,e_qs_ps_C_orig,e_qo_ps_C_orig See `ReboundTools::orig_vars`.
#' @param eta_engr_units_star,E_emb_star,t_life_star,C_cap_star,t_own_star,C_dot_md_star,eta_star,eta_ratio,S_dot_dev,G_dot,p_s_star,q_dot_s_star,C_dot_cap_star,E_dot_emb_star,C_dot_s_star,M_dot_star,N_dot_star,C_dot_o_star,f_Cs_star,e_qs_ps_UC_star,e_qo_ps_UC_star,e_qs_ps_C_star,e_qo_ps_C_star,E_dot_s_star See `ReboundTools::star_vars`.
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
                      MJ_engr_unit = ReboundTools::eeu_base_params$MJ_engr_unit,
                      
                      p_E = ReboundTools::orig_vars$p_E,
                      eta_orig = ReboundTools::orig_vars$eta_orig,
                      E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
                      q_dot_s_orig = ReboundTools::orig_vars$q_dot_s_orig,
                      M_dot_orig = ReboundTools::orig_vars$M_dot_orig,
                      C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig,
                      C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                      C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                      e_qs_ps_UC_orig = ReboundTools::orig_vars$e_qs_ps_UC_orig,
                      e_qo_ps_UC_orig = ReboundTools::orig_vars$e_qo_ps_UC_orig,
                      e_qs_ps_C_orig = ReboundTools::orig_vars$e_qs_ps_C_orig,
                      e_qo_ps_C_orig = ReboundTools::orig_vars$e_qo_ps_C_orig,
                      
                      eta_engr_units_star = ReboundTools::star_vars$eta_engr_units_star,
                      E_emb_star = ReboundTools::star_vars$E_emb_star,
                      t_life_star = ReboundTools::star_vars$t_life_star,
                      C_cap_star = ReboundTools::star_vars$C_cap_star,
                      t_own_star = ReboundTools::star_vars$t_own_star,
                      C_dot_md_star = ReboundTools::star_vars$C_dot_md_star,
                      
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
                      f_Cs_star = ReboundTools::star_vars$f_Cs_star,
                      e_qs_ps_UC_star = ReboundTools::star_vars$e_qs_ps_UC_star,
                      e_qo_ps_UC_star = ReboundTools::star_vars$e_qo_ps_UC_star,
                      e_qs_ps_C_star = ReboundTools::star_vars$e_qs_ps_C_star,
                      e_qo_ps_C_star = ReboundTools::star_vars$e_qo_ps_C_star,
                      E_dot_s_star = ReboundTools::star_vars$E_dot_s_star) {
  
  
  calc_star_fun <- function(MJ_engr_unit_val, 
                            eta_orig_val,
                            eta_engr_units_star_val,
                            E_dot_s_orig_val, 
                            p_E_val, 
                            q_dot_s_orig_val, 
                            C_cap_star_val,
                            t_own_star_val,
                            E_emb_star_val,
                            t_life_star_val,
                            M_dot_orig_val, 
                            C_dot_cap_orig_val,
                            C_dot_md_orig_val,
                            C_dot_md_star_val, 
                            C_dot_o_orig_val,
                            e_qs_ps_UC_orig_val,
                            e_qo_ps_UC_orig_val, 
                            e_qs_ps_C_orig_val,
                            e_qo_ps_C_orig_val) {
    
    eta_star_val <- eta_engr_units_star_val / MJ_engr_unit_val
    eta_ratio_val <- eta_star_val / eta_orig_val
    S_dot_dev_val <- (eta_ratio_val - 1) * (1/eta_ratio_val) * E_dot_s_orig_val
    G_dot_val <- p_E_val * S_dot_dev_val
    p_s_star_val <- p_E_val / eta_star_val
    q_dot_s_star_val <- q_dot_s_orig_val
    C_dot_cap_star_val <- C_cap_star_val / t_own_star_val
    E_dot_emb_star_val <- E_emb_star_val / t_life_star_val
    C_dot_s_star_val <- p_s_star_val * q_dot_s_star_val
    M_dot_star_val <- M_dot_orig_val
    N_dot_star_val <- G_dot_val - (C_dot_cap_star_val - C_dot_cap_orig_val) - (C_dot_md_star_val - C_dot_md_orig_val)
    C_dot_o_star_val <- C_dot_o_orig_val
    E_dot_s_star_val <- q_dot_s_star_val / eta_star_val
    f_Cs_star_val <- C_dot_s_star_val / (C_dot_s_star_val + C_dot_o_star_val)
    # Price elasticities at star point are same as elasticities at the orig point,
    # because we have not moved in the consumption plane.
    e_qs_ps_UC_star_val <- e_qs_ps_UC_orig_val
    e_qo_ps_UC_star_val <- e_qo_ps_UC_orig_val 
    e_qs_ps_C_star_val <- e_qs_ps_C_orig_val
    e_qo_ps_C_star_val <- e_qo_ps_C_orig_val

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
         E_dot_s_star_val, 
         f_Cs_star_val,
         e_qs_ps_UC_star_val,
         e_qo_ps_UC_star_val,
         e_qs_ps_C_star_val,
         e_qo_ps_C_star_val) %>% 
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
                            E_dot_s_star, 
                            f_Cs_star, 
                            e_qs_ps_UC_star,
                            e_qo_ps_UC_star,
                            e_qs_ps_C_star,
                            e_qo_ps_C_star))
  }
  
  matsindf::matsindf_apply(.orig_data, FUN = calc_star_fun, 
                           MJ_engr_unit_val = MJ_engr_unit,
                           eta_orig_val = eta_orig,
                           eta_engr_units_star_val = eta_engr_units_star,
                           E_dot_s_orig_val = E_dot_s_orig,
                           p_E_val = p_E, 
                           q_dot_s_orig_val = q_dot_s_orig, 
                           C_cap_star_val = C_cap_star,
                           t_own_star_val = t_own_star,
                           E_emb_star_val = E_emb_star,
                           t_life_star_val = t_life_star,
                           M_dot_orig_val = M_dot_orig,
                           C_dot_cap_orig_val = C_dot_cap_orig,
                           C_dot_md_orig_val = C_dot_md_orig,
                           C_dot_md_star_val = C_dot_md_star,
                           C_dot_o_orig_val = C_dot_o_orig, 
                           e_qs_ps_UC_orig_val = e_qs_ps_UC_orig,
                           e_qo_ps_UC_orig_val = e_qo_ps_UC_orig, 
                           e_qs_ps_C_orig_val = e_qs_ps_C_orig,
                           e_qo_ps_C_orig_val = e_qo_ps_C_orig)
}


#' Calculate energy rebound data at the hat stage
#' 
#' This function calculates energy rebound information for the hat
#' stage (immediately after the substitution effect).
#' 
#' By default, this function uses an exact method for calculating
#' the substitution effect. 
#' The exact method uses a constant elasticity of substitution (CES) 
#' formulation of utility, 
#' which assumes constant elasticity of substitution (sigma) along 
#' the indifference curve.
#'  
#' The approximate method is employed with
#' `use_sub_approx = TRUE`.
#' The approximate method 
#' assumes constant price elasticity of energy service consumption.
#' (In fact, the price elasticity of energy service consumption
#' is not constant along an indifference curve.)
#' The approximation leads to a simple and elegant rebound expression
#' but a different (and incorrect) consumption bundle after the substitution effect.
#'
#' @param .star_data An optional data frame containing EEU base data, original data, 
#'                   and star data, 
#'                   likely calculated by `calc_star()`.
#' @param use_sub_approx Tells whether to use an approximate method for 
#'                       calculating the substitution effect. 
#'                       Default is `FALSE`.
#'                       See details.
#' @param p_E See `ReboundTools::eeu_base_params`.
#' @param e_qs_M,e_qo_M See `ReboundTools::eeu_base_params`.
#' @param e_qo_ps_C,e_qs_ps_C,C_dot_cap_orig,C_dot_md_orig,f_Cs_orig,q_dot_s_orig,C_dot_o_orig,sigma,rho See `ReboundTools::orig_vars`.
#' @param eta_engr_units_star,eta_star,p_s_star,C_dot_cap_star,C_dot_md_star,E_dot_emb_star,M_dot_star,q_dot_s_star,eta_ratio,C_dot_o_star,e_qs_ps_UC_star,e_qo_ps_UC_star,e_qs_ps_C_star,e_qo_ps_C_star,N_dot_star,E_dot_s_star,G_dot See `ReboundTools::star_vars`.
#' @param eta_engr_units_hat,eta_hat,p_s_hat,C_dot_cap_hat,C_dot_md_hat,E_dot_emb_hat,M_dot_hat,q_dot_s_hat,E_dot_s_hat,C_dot_s_hat,C_dot_o_hat,f_Cs_hat,e_qs_ps_UC_hat,e_qo_ps_UC_hat,e_qs_ps_C_hat,e_qo_ps_C_hat,N_dot_hat,M_dot_hat_prime See `ReboundTools::hat_vars`.
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
                     use_sub_approx = FALSE,
                     # Input names
                     e_qs_M = ReboundTools::eeu_base_params$e_qs_M,
                     e_qo_M = ReboundTools::eeu_base_params$e_qo_M,

                     p_E = ReboundTools::orig_vars$p_E,
                     e_qo_ps_C = ReboundTools::orig_vars$e_qo_ps_C,
                     e_qs_ps_C = ReboundTools::orig_vars$e_qs_ps_C,
                     C_dot_cap_orig = ReboundTools::orig_vars$C_dot_cap_orig,
                     C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
                     f_Cs_orig = ReboundTools::orig_vars$f_Cs_orig,
                     q_dot_s_orig = ReboundTools::orig_vars$q_dot_s_orig,
                     C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                     sigma = ReboundTools::orig_vars$sigma,
                     rho = ReboundTools::orig_vars$rho,
                       
                     eta_engr_units_star = ReboundTools::star_vars$eta_engr_units_star,
                     eta_star = ReboundTools::star_vars$eta_star,
                     p_s_star = ReboundTools::star_vars$p_s_star,
                     C_dot_cap_star = ReboundTools::star_vars$C_dot_cap_star,
                     C_dot_md_star = ReboundTools::star_vars$C_dot_md_star,
                     E_dot_emb_star = ReboundTools::star_vars$E_dot_emb_star,
                     M_dot_star = ReboundTools::star_vars$M_dot_star,
                     q_dot_s_star = ReboundTools::star_vars$q_dot_s_star,
                     eta_ratio = ReboundTools::star_vars$eta_ratio,
                     C_dot_o_star = ReboundTools::star_vars$C_dot_o_star,
                     e_qs_ps_UC_star = ReboundTools::star_vars$e_qs_ps_UC_star,
                     e_qo_ps_UC_star = ReboundTools::star_vars$e_qo_ps_UC_star,
                     e_qs_ps_C_star = ReboundTools::star_vars$e_qs_ps_C_star,
                     e_qo_ps_C_star = ReboundTools::star_vars$e_qo_ps_C_star,
                     N_dot_star = ReboundTools::star_vars$N_dot_star,
                     E_dot_s_star = ReboundTools::star_vars$E_dot_s_star,
                     G_dot = ReboundTools::star_vars$G_dot,
                     # Output names
                     eta_engr_units_hat = ReboundTools::hat_vars$eta_engr_units_hat,
                     eta_hat = ReboundTools::hat_vars$eta_hat, 
                     p_s_hat = ReboundTools::hat_vars$p_s_hat,
                     C_dot_cap_hat = ReboundTools::hat_vars$C_dot_cap_hat,
                     C_dot_md_hat = ReboundTools::hat_vars$C_dot_md_hat,
                     E_dot_emb_hat = ReboundTools::hat_vars$E_dot_emb_hat,
                     M_dot_hat = ReboundTools::hat_vars$M_dot_hat,
                     q_dot_s_hat = ReboundTools::hat_vars$q_dot_s_hat,
                     E_dot_s_hat = ReboundTools::hat_vars$E_dot_s_hat,
                     C_dot_s_hat = ReboundTools::hat_vars$C_dot_s_hat,
                     C_dot_o_hat = ReboundTools::hat_vars$C_dot_o_hat,
                     f_Cs_hat = ReboundTools::hat_vars$f_Cs_hat,
                     e_qs_ps_UC_hat = ReboundTools::hat_vars$e_qs_ps_UC_hat,
                     e_qo_ps_UC_hat = ReboundTools::hat_vars$e_qo_ps_UC_hat,
                     e_qs_ps_C_hat = ReboundTools::hat_vars$e_qs_ps_C_hat,
                     e_qo_ps_C_hat = ReboundTools::hat_vars$e_qo_ps_C_hat,
                     N_dot_hat = ReboundTools::hat_vars$N_dot_hat, 
                     M_dot_hat_prime = ReboundTools::hat_vars$M_dot_hat_prime) {
  
  calc_hat_fun <- function(e_qs_M_val, 
                           e_qo_M_val, 
                           eta_engr_units_star_val,
                           eta_star_val,
                           p_s_star_val,
                           C_dot_cap_star_val,
                           C_dot_md_star_val,
                           E_dot_emb_star_val,
                           M_dot_star_val,
                           f_Cs_orig_val,
                           q_dot_s_orig_val,
                           C_dot_o_orig_val,
                           sigma_val, 
                           rho_val,
                           q_dot_s_star_val,
                           eta_ratio_val,
                           e_qs_ps_C_star_val,
                           e_qo_ps_C_star_val,
                           e_qs_ps_UC_star_val,
                           e_qo_ps_UC_star_val,
                           C_dot_o_star_val,
                           N_dot_star_val,
                           p_E_val,
                           E_dot_s_star_val,
                           C_dot_cap_orig_val,
                           C_dot_md_orig_val,
                           G_dot_val) {
    eta_engr_units_hat_val <- eta_engr_units_star_val
    eta_hat_val <- eta_star_val
    p_s_hat_val <- p_s_star_val
    C_dot_cap_hat_val <- C_dot_cap_star_val
    C_dot_md_hat_val <- C_dot_md_star_val
    E_dot_emb_hat_val <- E_dot_emb_star_val
    M_dot_hat_val <- M_dot_star_val
    
    if (use_sub_approx) {
      # This is the approximate expression for q_dot_s_hat.
      q_dot_s_hat_val <- q_dot_s_star_val * eta_ratio_val^(-e_qs_ps_C_star_val)
    } else {
      # Here is the exact expression for q_dot_s_hat
      # Preliminary calculations to make the actual expression easier to debug.
      a <- f_Cs_orig_val # Simpler variable name
      x <- p_s_star_val * q_dot_s_orig_val / C_dot_o_orig_val # dimensionless energy service price
      a_ratio <- (1-a) / a
      inv_a_ratio <- a / (1-a) # Inverse of a_ratio
      rho_ratio <- (1-rho_val) / rho_val
      inv_rho_ratio <- rho_val / (1-rho_val) # Inverse of rho_ratio
      
      # Q_s_hat_val is the dimensionless q_dot_s_hat defined as q_dot_s_hat / q_dot_s_orig
      Q_s_hat_val <- ( a + (1 - a) * ( (a_ratio*x)^(inv_rho_ratio) ) ) ^ (-1/rho_val) 
      # Recover q_dot_s_hat by multiplying by q_dot_s_orig.
      q_dot_s_hat_val <- Q_s_hat_val * q_dot_s_orig_val
    }

    E_dot_s_hat_val <- q_dot_s_hat_val / eta_hat_val
    C_dot_s_hat_val <- p_s_hat_val * q_dot_s_hat_val

    if (use_sub_approx) {
      # This is the approximate expression for C_dot_o_hat.
      C_dot_o_hat_val <- C_dot_o_star_val * eta_ratio_val^(-e_qo_ps_C_star_val)
    } else {
      # Here is the exact expression for C_dot_o_hat
      # C_o_hat_val is the dimensionless C_dot_o_hat defined as C_dot_o_hat / C_dot_o_orig
      # This is the original derived equation
      # C_o_hat_val <- ( 1/(1-a) - inv_a_ratio * (a + (1 - a) * (a_ratio*x)^inv_rho_ratio) ^ (-1) ) ^ (1/rho)
      # Wolfram alpha (correctly) says it can be simplified to the following:
      term <- x * (1-a) / a
      denom <- 1 + a*(term^(1-sigma_val)  - 1)
      C_o_hat_val <- (1 / denom)^(1/rho_val)
      # Recover C_dot_o_hat by multiplying by C_dot_o_orig
      C_dot_o_hat_val <- C_o_hat_val * C_dot_o_orig_val
    }

    f_Cs_hat_val <- C_dot_s_hat_val / (C_dot_s_hat_val + C_dot_o_hat_val)
    
    # Elasticities
    if (use_sub_approx) {
      # In the approximate utility model,
      # elasticities are assumed constant across the substitution effect
      # between the star and hat stages.
      e_qs_ps_C_hat_val <- e_qs_ps_C_star_val
      e_qo_ps_C_hat_val <- e_qo_ps_C_star_val
      e_qs_ps_UC_hat_val <- e_qs_ps_UC_star_val
      e_qo_ps_UC_hat_val <- e_qo_ps_UC_star_val
    } else {
      # In the exact utility model, 
      # we need to do more calculating.
      f <- f_Cs_orig_val
      g <- 1 - f
      h <- q_dot_s_orig_val / C_dot_o_orig_val
      m_o <- rho_val / (rho_val - 1)
      m_s <- rho_val / (1 - rho_val)
      n <- - 1/rho_val
      z <- g/f * h
      zpsms <- (z * p_s_hat_val)^m_s
      zpsmo <- (z * p_s_hat_val)^m_o
      e_qs_ps_C_hat_val <- (m_s * n * g * zpsms) / (f + g*zpsms)
      e_qo_ps_C_hat_val <- (m_o * n * f * zpsmo) / (1 + f*(zpsmo - 1))
      e_qs_ps_UC_hat_val <- e_qs_ps_C_hat_val - f_Cs_hat_val * e_qs_M_val
      e_qo_ps_UC_hat_val <- f_Cs_hat_val * (sigma_val - e_qo_M_val)
    }
      
    N_dot_hat_val <- N_dot_star_val - p_E_val*(E_dot_s_hat_val - E_dot_s_star_val) - (C_dot_o_hat_val - C_dot_o_star_val)
    M_dot_hat_prime_val <- M_dot_hat_val - C_dot_cap_star_val - C_dot_md_star_val - N_dot_hat_val
    
    list(eta_engr_units_hat_val,
         eta_hat_val,
         p_s_hat_val,
         C_dot_cap_hat_val,
         C_dot_md_hat_val,
         E_dot_emb_hat_val,
         M_dot_hat_val,
         q_dot_s_hat_val,
         E_dot_s_hat_val,
         C_dot_s_hat_val,
         C_dot_o_hat_val,
         f_Cs_hat_val,
         e_qs_ps_C_hat_val, 
         e_qo_ps_C_hat_val,
         e_qs_ps_UC_hat_val, 
         e_qo_ps_UC_hat_val,
         N_dot_hat_val,
         M_dot_hat_prime_val) %>%
      magrittr::set_names(c(eta_engr_units_hat,
                            eta_hat,
                            p_s_hat,
                            C_dot_cap_hat,
                            C_dot_md_hat,
                            E_dot_emb_hat,
                            M_dot_hat,
                            q_dot_s_hat,
                            E_dot_s_hat,
                            C_dot_s_hat,
                            C_dot_o_hat,
                            f_Cs_hat,
                            e_qs_ps_C_hat, 
                            e_qo_ps_C_hat,
                            e_qs_ps_UC_hat, 
                            e_qo_ps_UC_hat,
                            N_dot_hat,
                            M_dot_hat_prime))
  }
  
  matsindf::matsindf_apply(.star_data, FUN = calc_hat_fun, 
                           e_qs_M_val = e_qs_M, 
                           e_qo_M_val = e_qo_M,
                           eta_engr_units_star_val = eta_engr_units_star,
                           eta_star_val = eta_star, 
                           p_s_star_val = p_s_star,
                           C_dot_cap_star_val = C_dot_cap_star,
                           C_dot_md_star_val = C_dot_md_star,
                           E_dot_emb_star_val = E_dot_emb_star,
                           M_dot_star_val = M_dot_star,
                           f_Cs_orig_val = f_Cs_orig,
                           q_dot_s_orig_val = q_dot_s_orig,
                           C_dot_o_orig_val = C_dot_o_orig,
                           sigma_val = sigma,
                           rho_val = rho,
                           q_dot_s_star_val = q_dot_s_star,
                           eta_ratio_val = eta_ratio,
                           e_qs_ps_C_star_val = e_qs_ps_C_star,
                           e_qo_ps_C_star_val = e_qo_ps_C_star,
                           e_qs_ps_UC_star_val = e_qs_ps_UC_star,
                           e_qo_ps_UC_star_val = e_qo_ps_UC_star,
                           C_dot_o_star_val = C_dot_o_star,
                           N_dot_star_val = N_dot_star,
                           p_E_val = p_E, 
                           E_dot_s_star_val = E_dot_s_star,
                           C_dot_cap_orig_val = C_dot_cap_orig,
                           C_dot_md_orig_val = C_dot_md_orig,
                           G_dot_val = G_dot)
}


#' Calculate energy rebound data at the bar stage
#' 
#' This function calculates energy rebound information for the bar
#' stage (immediately after the income effect).
#'
#' @param .hat_data An optional data frame containing rebound calculations, original data, 
#'                  star data, and hat data,
#'                  likely calculated by `calc_hat()`.
#' @param tol The tolerance with which the budget constraint should be satisfied. Default is `1e-6`.
#' @param e_qs_M,e_qo_M,p_E See `ReboundTools::eeu_base_params`.
#' @param eta_engr_units_hat,eta_hat,p_s_hat,C_dot_cap_hat,C_dot_md_hat,E_dot_emb_hat,M_dot_hat,q_dot_s_hat,N_dot_hat,M_dot_hat_prime,C_dot_o_hat,e_qs_ps_UC_hat,e_qo_ps_UC_hat,e_qs_ps_C_hat,e_qo_ps_C_hat,E_dot_s_hat See `ReboundTools::hat_vars`.
#' @param eta_engr_units_bar,eta_bar,p_s_bar,C_dot_cap_bar,C_dot_md_bar,E_dot_emb_bar,M_dot_bar,q_dot_s_bar,E_dot_s_bar,C_dot_s_bar,C_dot_o_bar,f_Cs_bar,e_qs_ps_UC_bar,e_qo_ps_UC_bar,e_qs_ps_C_bar,e_qo_ps_C_bar,N_dot_bar See `ReboundTools::bar_vars`.
#' 
#' @return A list or data frame of derived rebound values for the bar stage (after the income effect).
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   calc_orig() %>% 
#'   calc_star() %>% 
#'   calc_hat() %>% 
#'   calc_bar()
calc_bar <- function(.hat_data = NULL,
                     tol = 1e-6,
                     # Input names
                     e_qs_M = ReboundTools::eeu_base_params$e_qs_M,
                     e_qo_M = ReboundTools::eeu_base_params$e_qo_M,
                     
                     p_E = ReboundTools::orig_vars$p_E, 
                     
                     eta_engr_units_hat = ReboundTools::hat_vars$eta_engr_units_hat,
                     eta_hat = ReboundTools::hat_vars$eta_hat,
                     p_s_hat = ReboundTools::hat_vars$p_s_hat,
                     C_dot_cap_hat = ReboundTools::hat_vars$C_dot_cap_hat,
                     C_dot_md_hat = ReboundTools::hat_vars$C_dot_md_hat,
                     E_dot_emb_hat = ReboundTools::hat_vars$E_dot_emb_hat,
                     M_dot_hat = ReboundTools::hat_vars$M_dot_hat,
                     q_dot_s_hat = ReboundTools::hat_vars$q_dot_s_hat,
                     N_dot_hat = ReboundTools::hat_vars$N_dot_hat,
                     M_dot_hat_prime = ReboundTools::hat_vars$M_dot_hat_prime,
                     C_dot_o_hat = ReboundTools::hat_vars$C_dot_o_hat,
                     e_qs_ps_UC_hat = ReboundTools::hat_vars$e_qs_ps_UC_hat,
                     e_qo_ps_UC_hat = ReboundTools::hat_vars$e_qo_ps_UC_hat,
                     e_qs_ps_C_hat = ReboundTools::hat_vars$e_qs_ps_C_hat,
                     e_qo_ps_C_hat = ReboundTools::hat_vars$e_qo_ps_C_hat,
                     E_dot_s_hat = ReboundTools::hat_vars$E_dot_s_hat,
                     
                     # Output names
                     eta_engr_units_bar = ReboundTools::bar_vars$eta_engr_units_bar,
                     eta_bar = ReboundTools::bar_vars$eta_bar,
                     p_s_bar = ReboundTools::bar_vars$p_s_bar,
                     C_dot_cap_bar = ReboundTools::bar_vars$C_dot_cap_bar,
                     C_dot_md_bar = ReboundTools::bar_vars$C_dot_md_bar,
                     E_dot_emb_bar = ReboundTools::bar_vars$E_dot_emb_bar,
                     M_dot_bar = ReboundTools::bar_vars$M_dot_bar,
                     q_dot_s_bar = ReboundTools::bar_vars$q_dot_s_bar,
                     E_dot_s_bar = ReboundTools::bar_vars$E_dot_s_bar,
                     C_dot_s_bar = ReboundTools::bar_vars$C_dot_s_bar,
                     C_dot_o_bar = ReboundTools::bar_vars$C_dot_o_bar,
                     f_Cs_bar = ReboundTools::bar_vars$f_Cs_bar,
                     e_qs_ps_UC_bar = ReboundTools::bar_vars$e_qs_ps_UC_bar,
                     e_qo_ps_UC_bar = ReboundTools::bar_vars$e_qo_ps_UC_bar,
                     e_qs_ps_C_bar = ReboundTools::bar_vars$e_qs_ps_C_bar,
                     e_qo_ps_C_bar = ReboundTools::bar_vars$e_qo_ps_C_bar,
                     N_dot_bar = ReboundTools::bar_vars$N_dot_bar
) {
  
  calc_bar_fun <- function(eta_engr_units_hat_val,
                           eta_hat_val, 
                           p_s_hat_val,
                           C_dot_cap_hat_val,
                           C_dot_md_hat_val,
                           E_dot_emb_hat_val,
                           M_dot_hat_val,
                           q_dot_s_hat_val,
                           N_dot_hat_val,
                           M_dot_hat_prime_val,
                           e_qs_M_val, 
                           C_dot_o_hat_val,
                           e_qo_M_val, 
                           p_E_val, 
                           E_dot_s_hat_val,
                           e_qs_ps_UC_hat_val,
                           e_qo_ps_UC_hat_val,
                           e_qs_ps_C_hat_val, 
                           e_qo_ps_C_hat_val) {
    eta_engr_units_bar_val <- eta_engr_units_hat_val
    eta_bar_val <- eta_hat_val
    p_s_bar_val <- p_s_hat_val
    C_dot_cap_bar_val <- C_dot_cap_hat_val
    C_dot_md_bar_val <- C_dot_md_hat_val
    E_dot_emb_bar_val <- E_dot_emb_hat_val
    M_dot_bar_val <- M_dot_hat_val
    q_dot_s_bar_val <- q_dot_s_hat_val * (1 + N_dot_hat_val/M_dot_hat_prime_val)^(e_qs_M_val)
    E_dot_s_bar_val <- q_dot_s_bar_val / eta_bar_val
    C_dot_s_bar_val <- p_s_bar_val * q_dot_s_bar_val
    C_dot_o_bar_val <- C_dot_o_hat_val * (1 + N_dot_hat_val/M_dot_hat_prime_val)^(e_qo_M_val)
    # N_dot_bar_val should be exactly 0. 
    # This will be true if 
    # N_dot_hat = p_E*(E_dot_s_bar - E_dot_s_hat) + (C_dot_o_bar - C_dot_o_hat)
    should_be_0 <- p_E_val*(E_dot_s_bar_val - E_dot_s_hat_val) + (C_dot_o_bar_val - C_dot_o_hat_val) - N_dot_hat_val
    assertthat::assert_that(all(abs(should_be_0) < tol))
    N_dot_bar_val <- rep(0, length(eta_hat_val))
    
    # Expenditure ratio
    f_Cs_bar_val <- C_dot_s_bar_val / (C_dot_s_bar_val + C_dot_o_bar_val) 
    
    # Elasticities are unchanged across the income effect
    e_qs_ps_UC_bar_val <- e_qs_ps_UC_hat_val
    e_qo_ps_UC_bar_val <- e_qo_ps_UC_hat_val
    e_qs_ps_C_bar_val <- e_qs_ps_C_hat_val
    e_qo_ps_C_bar_val <- e_qo_ps_C_hat_val
    
    list(eta_engr_units_bar_val,
         eta_bar_val,
         p_s_bar_val,
         C_dot_cap_bar_val,
         C_dot_md_bar_val,
         E_dot_emb_bar_val,
         M_dot_bar_val, 
         q_dot_s_bar_val,
         E_dot_s_bar_val,
         C_dot_s_bar_val,
         C_dot_o_bar_val,
         f_Cs_bar_val,
         e_qs_ps_UC_bar_val, 
         e_qo_ps_UC_bar_val, 
         e_qs_ps_C_bar_val,
         e_qo_ps_C_bar_val,
         N_dot_bar_val) %>% 
      magrittr::set_names(c(eta_engr_units_bar,
                            eta_bar,
                            p_s_bar, 
                            C_dot_cap_bar,
                            C_dot_md_bar, 
                            E_dot_emb_bar,
                            M_dot_bar, 
                            q_dot_s_bar,
                            E_dot_s_bar,
                            C_dot_s_bar,
                            C_dot_o_bar, 
                            f_Cs_bar,
                            e_qs_ps_UC_bar,
                            e_qo_ps_UC_bar,
                            e_qs_ps_C_bar, 
                            e_qo_ps_C_bar, 
                            N_dot_bar))
  }
  
  matsindf::matsindf_apply(.hat_data, FUN = calc_bar_fun,
                           eta_engr_units_hat_val = eta_engr_units_hat,
                           eta_hat_val = eta_hat, 
                           p_s_hat_val = p_s_hat,
                           C_dot_cap_hat_val = C_dot_cap_hat,
                           C_dot_md_hat_val = C_dot_md_hat, 
                           E_dot_emb_hat_val = E_dot_emb_hat,
                           M_dot_hat_val = M_dot_hat,
                           q_dot_s_hat_val = q_dot_s_hat,
                           N_dot_hat_val = N_dot_hat,
                           M_dot_hat_prime_val = M_dot_hat_prime,
                           e_qs_M_val = e_qs_M, 
                           C_dot_o_hat_val = C_dot_o_hat,
                           e_qo_M_val = e_qo_M,
                           p_E_val = p_E,
                           E_dot_s_hat_val = E_dot_s_hat,
                           e_qs_ps_UC_hat_val = e_qs_ps_UC_hat,
                           e_qo_ps_UC_hat_val = e_qo_ps_UC_hat,
                           e_qs_ps_C_hat_val = e_qs_ps_C_hat,
                           e_qo_ps_C_hat_val = e_qo_ps_C_hat)
}


#' Calculate energy rebound data at the tilde stage
#' 
#' This function calculates energy rebound information for the tilde
#' stage (immediately after the macro effect).
#'
#' @param .bar_data An optional data frame containing rebound calculations, original data, 
#'                  star data, hat data, and bar data,
#'                  likely calculated by `calc_bar()`.
#' @param eta_engr_units_bar,eta_bar,p_s_bar,C_dot_cap_bar,C_dot_md_bar,E_dot_emb_bar,M_dot_bar,q_dot_s_bar,E_dot_s_bar,C_dot_s_bar,C_dot_o_bar,e_qs_ps_UC_bar,e_qo_ps_UC_bar,e_qs_ps_C_bar,e_qo_ps_C_bar,N_dot_bar See `ReboundTools::bar_vars`.
#' @param eta_engr_units_tilde,eta_tilde,p_s_tilde,C_dot_cap_tilde,C_dot_md_tilde,E_dot_emb_tilde,M_dot_tilde,q_dot_s_tilde,E_dot_s_tilde,C_dot_s_tilde,C_dot_o_tilde,f_Cs_tilde,e_qs_ps_UC_tilde,e_qo_ps_UC_tilde,e_qs_ps_C_tilde,e_qo_ps_C_tilde,N_dot_tilde See `ReboundTools::tilde_vars`.
#'
#' @return A list or data frame of derived rebound values for the bar stage (after the income effect).
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   calc_orig() %>% 
#'   calc_star() %>% 
#'   calc_hat() %>% 
#'   calc_bar() %>% 
#'   calc_tilde()
calc_tilde <- function(.bar_data = NULL,
                       # Input names
                       eta_engr_units_bar = ReboundTools::bar_vars$eta_engr_units_bar,
                       eta_bar = ReboundTools::bar_vars$eta_bar,
                       p_s_bar = ReboundTools::bar_vars$p_s_bar,
                       C_dot_cap_bar = ReboundTools::bar_vars$C_dot_cap_bar,
                       C_dot_md_bar = ReboundTools::bar_vars$C_dot_md_bar,
                       E_dot_emb_bar = ReboundTools::bar_vars$E_dot_emb_bar,
                       M_dot_bar = ReboundTools::bar_vars$M_dot_bar,
                       q_dot_s_bar = ReboundTools::bar_vars$q_dot_s_bar,
                       E_dot_s_bar = ReboundTools::bar_vars$E_dot_s_bar,
                       C_dot_s_bar = ReboundTools::bar_vars$C_dot_s_bar,
                       C_dot_o_bar = ReboundTools::bar_vars$C_dot_o_bar,
                       e_qs_ps_UC_bar = ReboundTools::bar_vars$e_qs_ps_UC_bar,
                       e_qo_ps_UC_bar = ReboundTools::bar_vars$e_qo_ps_UC_bar,
                       e_qs_ps_C_bar = ReboundTools::bar_vars$e_qs_ps_C_bar,
                       e_qo_ps_C_bar = ReboundTools::bar_vars$e_qo_ps_C_bar,
                       N_dot_bar = ReboundTools::bar_vars$N_dot_bar,
                       
                       # Output names
                       eta_engr_units_tilde = ReboundTools::tilde_vars$eta_engr_units_tilde,
                       eta_tilde = ReboundTools::tilde_vars$eta_tilde,
                       p_s_tilde = ReboundTools::tilde_vars$p_s_tilde,
                       C_dot_cap_tilde = ReboundTools::tilde_vars$C_dot_cap_tilde,
                       C_dot_md_tilde = ReboundTools::tilde_vars$C_dot_md_tilde,
                       E_dot_emb_tilde = ReboundTools::tilde_vars$E_dot_emb_tilde,
                       M_dot_tilde = ReboundTools::tilde_vars$M_dot_tilde,
                       q_dot_s_tilde = ReboundTools::tilde_vars$q_dot_s_tilde,
                       E_dot_s_tilde = ReboundTools::tilde_vars$E_dot_s_tilde,
                       C_dot_s_tilde = ReboundTools::tilde_vars$C_dot_s_tilde,
                       C_dot_o_tilde = ReboundTools::tilde_vars$C_dot_o_tilde,
                       f_Cs_tilde = ReboundTools::tilde_vars$f_Cs_tilde,
                       e_qs_ps_UC_tilde = ReboundTools::tilde_vars$e_qs_ps_UC_tilde,
                       e_qo_ps_UC_tilde = ReboundTools::tilde_vars$e_qo_ps_UC_tilde,
                       e_qs_ps_C_tilde = ReboundTools::tilde_vars$e_qs_ps_C_tilde,
                       e_qo_ps_C_tilde = ReboundTools::tilde_vars$e_qo_ps_C_tilde,
                       N_dot_tilde = ReboundTools::tilde_vars$N_dot_tilde
) {
  
  calc_tilde_fun <- function(eta_engr_units_bar_val,
                             eta_bar_val, 
                             p_s_bar_val,
                             C_dot_cap_bar_val,
                             C_dot_md_bar_val,
                             E_dot_emb_bar_val,
                             M_dot_bar_val,
                             q_dot_s_bar_val,
                             E_dot_s_bar_val,
                             C_dot_s_bar_val,
                             C_dot_o_bar_val,
                             e_qs_ps_UC_bar_val,
                             e_qo_ps_UC_bar_val,
                             e_qs_ps_C_bar_val, 
                             e_qo_ps_C_bar_val,
                             N_dot_bar_val) {
    eta_engr_units_tilde_val <- eta_engr_units_bar_val
    eta_tilde_val <- eta_bar_val
    p_s_tilde_val <- p_s_bar_val
    C_dot_cap_tilde_val <- C_dot_cap_bar_val
    C_dot_md_tilde_val <- C_dot_md_bar_val
    E_dot_emb_tilde_val <- E_dot_emb_bar_val
    M_dot_tilde_val <- M_dot_bar_val
    q_dot_s_tilde_val <- q_dot_s_bar_val
    E_dot_s_tilde_val <- E_dot_s_bar_val
    C_dot_s_tilde_val <- C_dot_s_bar_val
    C_dot_o_tilde_val <- C_dot_o_bar_val
    # Expenditure fraction
    f_Cs_tilde_val <- C_dot_s_tilde_val / (C_dot_s_tilde_val + C_dot_o_tilde_val)
    # Elasticities are unchanged across the macro effect
    e_qs_ps_UC_tilde_val <- e_qs_ps_UC_bar_val
    e_qo_ps_UC_tilde_val <- e_qo_ps_UC_bar_val
    e_qs_ps_C_tilde_val <- e_qs_ps_C_bar_val
    e_qo_ps_C_tilde_val <- e_qo_ps_C_bar_val
    
    N_dot_tilde_val <- N_dot_bar_val
    
    list(eta_engr_units_tilde_val,
         eta_tilde_val,
         p_s_tilde_val,
         C_dot_cap_tilde_val,
         C_dot_md_tilde_val,
         E_dot_emb_tilde_val,
         M_dot_tilde_val, 
         q_dot_s_tilde_val,
         E_dot_s_tilde_val,
         C_dot_s_tilde_val,
         C_dot_o_tilde_val,
         f_Cs_tilde_val,
         e_qs_ps_UC_tilde_val,
         e_qo_ps_UC_tilde_val,
         e_qs_ps_C_tilde_val,
         e_qo_ps_C_tilde_val,
         N_dot_tilde_val) %>% 
      magrittr::set_names(c(eta_engr_units_tilde,
                            eta_tilde,
                            p_s_tilde, 
                            C_dot_cap_tilde,
                            C_dot_md_tilde, 
                            E_dot_emb_tilde,
                            M_dot_tilde, 
                            q_dot_s_tilde,
                            E_dot_s_tilde,
                            C_dot_s_tilde,
                            C_dot_o_tilde, 
                            f_Cs_tilde,
                            e_qs_ps_UC_tilde,
                            e_qo_ps_UC_tilde,
                            e_qs_ps_C_tilde,
                            e_qo_ps_C_tilde,
                            N_dot_tilde))
  }
  
  matsindf::matsindf_apply(.bar_data, FUN = calc_tilde_fun,
                           eta_engr_units_bar_val = eta_engr_units_bar, 
                           eta_bar_val = eta_bar, 
                           p_s_bar_val = p_s_bar,
                           C_dot_cap_bar_val = C_dot_cap_bar,
                           C_dot_md_bar_val = C_dot_md_bar, 
                           E_dot_emb_bar_val = E_dot_emb_bar,
                           M_dot_bar_val = M_dot_bar,
                           q_dot_s_bar_val = q_dot_s_bar,
                           E_dot_s_bar_val = E_dot_s_bar,
                           C_dot_s_bar_val = C_dot_s_bar,
                           C_dot_o_bar_val = C_dot_o_bar,
                           e_qs_ps_UC_bar_val = e_qs_ps_UC_bar,
                           e_qo_ps_UC_bar_val = e_qo_ps_UC_bar,
                           e_qs_ps_C_bar_val = e_qs_ps_C_bar,
                           e_qo_ps_C_bar_val =  e_qo_ps_C_bar,
                           N_dot_bar_val = N_dot_bar)
}


#' Calculate differences between stages
#'
#' @param .tilde_data A data frame containing rebound calculations, original data, 
#'                    star data, hat data, and tilde data,
#'                    likely calculated by `calc_tilde()`.
#' @param key_analysis_vars See `ReboundTools::key_analysis_vars`.
#' @param rebound_stages See `ReboundTools::rebound_stages`.
#' @param .all_vars A column names used internally for all variables.
#'                  Default is ".all_vars".
#'
#' @return `.tilde_data` with additional columns containing all possible difference terms.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   calc_orig() %>% 
#'   calc_star() %>% 
#'   calc_hat() %>% 
#'   calc_bar() %>% 
#'   calc_tilde() %>% 
#'   calc_Deltas()
calc_Deltas <- function(.tilde_data = NULL, 
                        key_analysis_vars = ReboundTools::key_analysis_vars,
                        rebound_stages = ReboundTools::rebound_stages, 
                        .all_vars = "all_vars") {
  
  # Eliminate the first stage, because we're dealing with Deltas between stages.
  vars <- expand.grid(key_analysis_vars, rebound_stages) %>% 
    magrittr::set_names(c("key_analysis_vars", "rebound_stages")) %>% 
    dplyr::mutate(
      "{.all_vars}" := paste0(.data[["key_analysis_vars"]], "_", .data[["rebound_stages"]])
    ) %>% 
    dplyr::select(dplyr::all_of(.all_vars))
  
  n_vars <- length(key_analysis_vars)
  n_stages <- length(rebound_stages)
  
  minuends <- vars[(n_vars+1):(n_vars*n_stages), ]
  subtrahends <- vars[1:(n_vars*(n_stages-1)), ]
  subtraction_df <- tibble::tibble(minuend = minuends, 
                                   subtrahend = subtrahends) %>% 
    dplyr::mutate(
      var_name = paste0("Delta_", .data[["minuend"]])
    )
  
  for (i in 1:nrow(subtraction_df)) {
    col_name <- subtraction_df[[i, "var_name"]]
    minuend_name <- subtraction_df[[i, "minuend"]]
    subtrahend_name <- subtraction_df[[i, "subtrahend"]]
    .tilde_data[[col_name]] <- .tilde_data[[minuend_name]] - .tilde_data[[subtrahend_name]]
  }  
  
  .tilde_data
}


#' Calculate rebound terms.
#' 
#' This function calculates rebound terms from a data frame that already contains Delta terms.
#' Note that each rebound term is calculated twice as a way of validating the 
#' derived expression for rebound.
#'
#' @param .Deltas_data A data frame containing Delta values, likely created by `ReboundTools::calc_Deltas()`
#' @param tol The tolerance for checking internal consistency of rebound calculations. Default is `1e-6`.
#' @param I_E,e_qs_M,e_qo_M,k See `ReboundTools::eeu_base_params`.
#' @param e_qs_ps_C,e_qo_ps_C,C_dot_o_orig,E_dot_s_orig See `ReboundTools::orig_vars`.
#' @param S_dot_dev,eta_ratio See `ReboundTools::star_vars`.
#' @param N_dot_star See `ReboundTools::star_vars`.
#' @param M_dot_hat_prime See `ReboundTools::hat_vars`.
#' @param Delta_E_dot_emb_star,Delta_C_dot_cap_star,Delta_C_dot_md_star,Delta_E_dot_s_hat,Delta_C_dot_o_hat,Delta_E_dot_s_bar,Delta_C_dot_o_bar See `ReboundTools::Delta_vars`.
#' @param Re_dempl,Re_emb,Re_cap,Re_md,Re_empl,Re_dsub,Re_isub,Re_sub,Re_dinc,Re_iinc,Re_inc,Re_micro,Re_macro,Re_dir,Re_indir,Re_tot See `ReboundTools::rebound_terms`.
#'
#' @return A data frame with rebound terms added as columns.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   calc_orig() %>% 
#'   calc_star() %>% 
#'   calc_hat() %>% 
#'   calc_bar() %>% 
#'   calc_tilde() %>% 
#'   calc_Deltas() %>% 
#'   calc_rebound()
calc_rebound <- function(.Deltas_data = NULL, 
                         tol = 1e-6,
                         # Input names
                         I_E = ReboundTools::eeu_base_params$I_E,
                         e_qs_M = ReboundTools::eeu_base_params$e_qs_M,
                         e_qo_M = ReboundTools::eeu_base_params$e_qo_M,
                         k = ReboundTools::eeu_base_params$k,
                         
                         e_qs_ps_C = ReboundTools::orig_vars$e_qs_ps_C,
                         e_qo_ps_C = ReboundTools::orig_vars$e_qo_ps_C,
                         C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
                         E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
                         
                         S_dot_dev = ReboundTools::star_vars$S_dot_dev, 
                         eta_ratio = ReboundTools::star_vars$eta_ratio, 
                         
                         N_dot_star = ReboundTools::star_vars$N_dot_star,
                         M_dot_hat_prime = ReboundTools::hat_vars$M_dot_hat_prime,
                         
                         Delta_E_dot_emb_star = ReboundTools::Delta_vars$Delta_E_dot_emb_star,
                         Delta_C_dot_cap_star = ReboundTools::Delta_vars$Delta_C_dot_cap_star,
                         Delta_C_dot_md_star = ReboundTools::Delta_vars$Delta_C_dot_md_star,
                         Delta_E_dot_s_hat = ReboundTools::Delta_vars$Delta_E_dot_s_hat,
                         Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
                         Delta_E_dot_s_bar = ReboundTools::Delta_vars$Delta_E_dot_s_bar,
                         Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar,
                         
                         # Output names
                         Re_dempl = ReboundTools::rebound_terms$Re_dempl,
                         Re_emb = ReboundTools::rebound_terms$Re_emb, 
                         Re_cap = ReboundTools::rebound_terms$Re_cap, 
                         Re_md = ReboundTools::rebound_terms$Re_md,
                         Re_empl = ReboundTools::rebound_terms$Re_empl,
                         Re_dsub = ReboundTools::rebound_terms$Re_dsub, 
                         Re_isub = ReboundTools::rebound_terms$Re_isub,
                         Re_sub = ReboundTools::rebound_terms$Re_sub,
                         Re_dinc = ReboundTools::rebound_terms$Re_dinc,
                         Re_iinc = ReboundTools::rebound_terms$Re_iinc,
                         Re_inc = ReboundTools::rebound_terms$Re_inc,
                         Re_micro = ReboundTools::rebound_terms$Re_micro,
                         Re_macro = ReboundTools::rebound_terms$Re_macro,
                         Re_dir = ReboundTools::rebound_terms$Re_dir,
                         Re_indir = ReboundTools::rebound_terms$Re_indir,
                         Re_tot = ReboundTools::rebound_terms$Re_tot) {
  
  rebound_fun <- function(Delta_E_dot_emb_star_val, 
                          S_dot_dev_val,
                          Delta_C_dot_cap_star_val,
                          Delta_C_dot_md_star_val, 
                          N_dot_star_val,
                          I_E_val,
                          eta_ratio_val, 
                          e_qs_ps_C_val,
                          Delta_E_dot_s_hat_val,
                          e_qo_ps_C_val,
                          C_dot_o_orig_val,
                          E_dot_s_orig_val,
                          Delta_C_dot_o_hat_val,
                          M_dot_hat_prime_val,
                          e_qs_M_val,
                          Delta_E_dot_s_bar_val, 
                          e_qo_M_val,
                          Delta_C_dot_o_bar_val, 
                          k_val
                          ) {
    # Direct emplacement rebound
    Re_dempl_val <- 0
    
    # Indirect embodied energy effect rebound. 
    # Note: this formulation avoids a division-by-zero error if E_dot_emb_orig = 0
    Re_emb_val <- Delta_E_dot_emb_star_val / S_dot_dev_val

    # Capital cost rebound
    Re_cap_val <- Delta_C_dot_cap_star_val * I_E_val / S_dot_dev_val
    
    # Indirect maintenance and disposal effect energy rebound
    # Note: this formulation avoids a division-by-zero error if C_dot_md_orig = 0
    Re_md_val <- Delta_C_dot_md_star_val * I_E_val / S_dot_dev_val
    
    # Emplacement effect rebound
    Re_empl_val <- Re_emb_val + Re_md_val
    
    # Indirect substitution effect rebound
    Re_isub_val <- Delta_C_dot_o_hat_val * I_E_val / S_dot_dev_val    

    # Direct substitution effect rebound
    Re_dsub_val <- Delta_E_dot_s_hat_val / S_dot_dev_val
    
    # Substitution effect rebound
    Re_sub_val = Re_dsub_val + Re_isub_val
    
    # Direct income effect rebound 
    Re_dinc_val <- Delta_E_dot_s_bar_val / S_dot_dev_val

    # Indirect income effect rebound 
    Re_iinc_val <- Delta_C_dot_o_bar_val * I_E_val / S_dot_dev_val

    # Income effect rebound
    Re_inc_val <- Re_dinc_val + Re_iinc_val
    
    # Sum of all micro rebound effects
    Re_micro_val <- Re_empl_val + Re_sub_val + Re_inc_val

    # Macro effect rebound
    Re_macro_val <- k_val * N_dot_star_val * I_E_val / S_dot_dev_val
    
    # Direct rebound
    Re_dir_val <- Re_dsub_val + Re_dinc_val
    
    # Indirect rebound
    Re_indir_val <- Re_emb_val + Re_md_val + Re_isub_val + Re_iinc_val + Re_macro_val
    
    # Total rebound
    Re_tot_val <- Re_dempl_val + Re_emb_val + Re_md_val + Re_dsub_val + Re_isub_val + Re_dinc_val + Re_iinc_val + Re_macro_val
    
    # Double-check the sums
    Re_tot_check <- Re_dir_val + Re_indir_val
    assertthat::assert_that(all(abs(Re_tot_check - Re_tot_val) < tol), msg = "Re_tot failed consistency check in calc_rebound().")
    
    list(Re_dempl_val,
         Re_emb_val,
         Re_cap_val,
         Re_md_val,
         Re_empl_val,
         Re_isub_val,
         Re_dsub_val,
         Re_sub_val,
         Re_dinc_val,
         Re_iinc_val,
         Re_inc_val,
         Re_micro_val,
         Re_macro_val,
         Re_dir_val,
         Re_indir_val,
         Re_tot_val) %>% 
      magrittr::set_names(c(Re_dempl,
                            Re_emb,
                            Re_cap,
                            Re_md,
                            Re_empl,
                            Re_isub,
                            Re_dsub,
                            Re_sub,
                            Re_dinc,
                            Re_iinc,
                            Re_inc,
                            Re_micro,
                            Re_macro,
                            Re_dir,
                            Re_indir,
                            Re_tot))
  }
  
  matsindf::matsindf_apply(.Deltas_data, FUN = rebound_fun, 
                           Delta_E_dot_emb_star_val = Delta_E_dot_emb_star,
                           S_dot_dev_val = S_dot_dev,
                           Delta_C_dot_cap_star_val = Delta_C_dot_cap_star,
                           Delta_C_dot_md_star_val = Delta_C_dot_md_star,
                           N_dot_star_val = N_dot_star,
                           I_E_val = I_E,
                           eta_ratio_val = eta_ratio, 
                           e_qs_ps_C_val = e_qs_ps_C,
                           Delta_E_dot_s_hat_val = Delta_E_dot_s_hat,
                           e_qo_ps_C_val = e_qo_ps_C,
                           C_dot_o_orig_val = C_dot_o_orig,
                           E_dot_s_orig_val = E_dot_s_orig,
                           Delta_C_dot_o_hat_val = Delta_C_dot_o_hat,
                           M_dot_hat_prime_val = M_dot_hat_prime,
                           e_qs_M_val = e_qs_M,
                           Delta_E_dot_s_bar_val = Delta_E_dot_s_bar,
                           e_qo_M_val = e_qo_M,
                           Delta_C_dot_o_bar_val = Delta_C_dot_o_bar,
                           k_val = k
  ) 
}


#' Perform a complete rebound analysis
#' 
#' This function calls all rebound analysis functions in the correct order
#'
#' @param .eeu_data Energy efficiency upgrade information in a data frame.
#'                  See `load_eeu_data()` for an example data frame.
#' @param use_sub_approx See [ReboundTools::calc_hat()].
#'
#' @return `.eeu_data` with all rebound terms added as columns to the right.
#' 
#' @export
#'
#' @examples
#' complicated <- load_eeu_data() %>% 
#'   calc_orig() %>% 
#'   calc_star() %>% 
#'   calc_hat() %>% 
#'   calc_bar() %>% 
#'   calc_tilde() %>% 
#'   calc_Deltas() %>% 
#'   calc_rebound()
#' simple <- load_eeu_data() %>% 
#'   rebound_analysis()
#' all.equal(complicated, simple)
rebound_analysis <- function(.eeu_data, use_sub_approx = FALSE) {
  .eeu_data %>% 
    calc_orig() %>% 
    calc_star() %>% 
    calc_hat(use_sub_approx = use_sub_approx) %>% 
    calc_bar() %>% 
    calc_tilde() %>% 
    calc_Deltas() %>% 
    calc_rebound()
}
