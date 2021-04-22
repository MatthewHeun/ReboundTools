#' Calibrate values for macro parameter k
#' 
#' The macro parameter `k` is the least-well-known parameter in rebound analysis.
#' `k` links micro and macro rebound effects, and can be calibrated if
#' macro rebound (`Re_macro`) is considered to be a residual and `k` selected 
#' such that a desired total rebound value (`target_Re_tot`) is obtained. 
#' 
#' The calibrated value of `k` is calculated by 
#' `S_dot_dev / (N_dot_hat * I_E) * [Re_macro + (Re_tot_target - Re_tot)]`.
#' 
#' Names of return values are taken from the case column in `.rebound_data`.
#'
#' @param .rebound_data A data frame of rebound data, probably created by `rebound_analysis()`.
#' @param Re_tot_target The target value for total rebound. 
#'                      A number or a vector of numbers of same length as the number of observations
#'                      in `.rebound_data`.
#' @param case_colname,I_E_colname See `ReboundTools::eeu_base_params`.
#' @param S_dot_dev_colname See `ReboundTools::star_vars`.
#' @param N_dot_hat_colname See `ReboundTools::hat_vars`.
#' @param Re_macro_colname,Re_tot_colname See `ReboundTools::rebound_terms`.
#'
#' @return A number or vector of numbers of $k$ values which make total rebound match `target_Re_tot`.
#' 
#' @export
#'
#' @examples
#' load_eeu_data() %>% 
#'   rebound_analysis() %>% 
#'   calibrate_k(0.63)
calibrate_k <- function(.rebound_data, Re_tot_target, 
                        case_colname = ReboundTools::eeu_base_params$case,
                        I_E_colname = ReboundTools::eeu_base_params$I_E, 
                        S_dot_dev_colname = ReboundTools::star_vars$S_dot_dev,
                        N_dot_hat_colname = ReboundTools::hat_vars$N_dot_hat, 
                        Re_macro_colname = ReboundTools::rebound_terms$Re_macro, 
                        Re_tot_colname = ReboundTools::rebound_terms$Re_tot) {
  .rebound_data[[S_dot_dev_colname]] / .rebound_data[[N_dot_hat_colname]] / .rebound_data[[I_E_colname]] * 
    (.rebound_data[[Re_macro_colname]] + Re_tot_target - .rebound_data[[Re_tot_colname]]) %>% 
    magrittr::set_names(.rebound_data[[case_colname]])
}