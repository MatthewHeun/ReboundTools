


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
                              eta_orig = ReboundTools::eeu_base_data$eta_orig,
                              eta_tilde = ReboundTools::eeu_base_data$eta_tilde,
                              # Output names
                              eta_ratio = ReboundTools::eeu_derived_data$eta_ratio) {

  rebound_calcs_fun <- function(eta_orig_val, eta_tilde_val) {
    eta_ratio_val <- eta_tilde_val / eta_orig_val
    
    
    list(eta_ratio_val) %>% magrittr::set_names(eta_ratio)
  }
  
  
  matsindf::matsindf_apply(.eeu_data, FUN = rebound_calcs_fun, 
                           eta_orig_val = eta_orig,
                           eta_tilde_val = eta_tilde)
  
}





