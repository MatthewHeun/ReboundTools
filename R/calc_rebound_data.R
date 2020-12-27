


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
calc_rebound_data <- function(.eeu_data = NULL,
                              # Input names
                              MJ_engr_unit = ReboundTools::eeu_base_data$MJ_engr_unit,
                              p_E = ReboundTools::eeu_base_data$p_E,
                              eta_orig_engr_units = ReboundTools::eeu_base_data$eta_orig_engr_units,
                              eta_tilde_engr_units = ReboundTools::eeu_base_data$eta_tilde_engr_units,
                              q_dot_s_orig = ReboundTools::eeu_base_data$q_dot_s_orig,
                              # Output names
                              eta_orig = ReboundTools::eeu_derived_data$eta_orig,
                              eta_tilde = ReboundTools::eeu_derived_data$eta_tilde,
                              eta_ratio = ReboundTools::eeu_derived_data$eta_ratio,
                              E_dot_s_orig = ReboundTools::eeu_derived_data$E_dot_s_orig,
                              S_dot_dev = ReboundTools::eeu_derived_data$S_dot_dev, 
                              G_dot = ReboundTools::eeu_derived_data$G_dot) {

  rebound_calcs_fun <- function(MJ_engr_unit_val, 
                                p_E_val,
                                eta_orig_engr_units_val, 
                                eta_tilde_engr_units_val, 
                                q_dot_s_orig_val) {
    eta_orig_val <- eta_orig_engr_units_val / MJ_engr_unit_val
    eta_tilde_val <- eta_tilde_engr_units_val / MJ_engr_unit_val
    eta_ratio_val <- eta_tilde_val / eta_orig_val
    E_dot_s_orig_val <- q_dot_s_orig_val / eta_orig_val
    S_dot_dev_val <- (eta_ratio_val - 1) * (1/eta_ratio_val) * E_dot_s_orig_val
    G_dot_val <- p_E_val * S_dot_dev_val
    
    
    list(eta_orig_val, 
         eta_tilde_val, 
         eta_ratio_val,
         E_dot_s_orig_val, 
         S_dot_dev_val, 
         G_dot_val) %>% magrittr::set_names(c(eta_orig, 
                                              eta_tilde, 
                                              eta_ratio, 
                                              E_dot_s_orig,
                                              S_dot_dev, 
                                              G_dot))
  }
  
  
  matsindf::matsindf_apply(.eeu_data, FUN = rebound_calcs_fun, 
                           MJ_engr_unit_val = MJ_engr_unit,
                           p_E_val = p_E,
                           eta_orig_engr_units_val = eta_orig_engr_units,
                           eta_tilde_engr_units_val = eta_tilde_engr_units,
                           q_dot_s_orig_val = q_dot_s_orig)
  
}





