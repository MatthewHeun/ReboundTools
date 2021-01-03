#' Units for rebound variables
#' 
#' This function calculates the units for rebound analysis variables.
#'
#' @param .rebound_data A data frame, typically calculated by `rebound_analysis()`.
#'                      Default is `load_eeu_data() %>% rebound_analysis()`.
#'
#' @return
#' 
#' @export
#'
#' @examples
#' var_units()
var_units <- function(.rebound_data = load_eeu_data() %>% rebound_analysis(), 
                      case = ReboundTools::eeu_base_params$case, 
                      service_unit = ReboundTools::eeu_base_params$service_unit,
                      energy_engr_unit = ReboundTools::eeu_base_params$energy_engr_unit) {
  vars <- colnames(.rebound_data) %>% 
    sub(x = ., pattern = "_[^_]*$", replacement = "") %>% 
    unique()
  
  units <- .rebound_data %>% 
    dplyr::select(vars(case, service_unit, ))
  
  
  
}




#' Units for variables
#'
#' @param .var_name 
#' @param service_unit 
#' @param energy_engr_unit 
#' @param leading_delta 
#' @param trailing_stage 
#' @param currency 
#' @param time 
#'
#' @return
#' @export
#'
#' @examples
units <- function(.var_name, service_unit, energy_engr_unit, 
                  surround_left = "[", 
                  surround_right = "]",
                  leading_delta = "^Delta_", 
                  trailing_stage = "_[^_]*$", 
                  energy_si = "MJ",
                  efficiency_engy_units = "eta_engr_units",
                  efficiency = "eta", 
                  cost_rate = "C_dot",
                  currency = "$", 
                  time = "year") {
  
  
  sapply(.var_name, function(var){
    # Get rid of leading "Delta_"
    v <- sub(pattern = leading_delta, replacement = "", var)
    # Get rid of trailing "_orig" and similar.
    v <- sub(pattern = trailing_stage, replacement = "", v)
    if (startsWith(v, cost_rate)) {
      out <- paste0(currency, "/", time)
    } else if (startsWith(v, efficiency_engy_units)) {
      out <- paste0(service_unit, "/", energy_engr_unit)
    }
    
    if (!is.null(surround_left) & !is.null(surround_right)) {
      out <- paste0(surround_left, out, surround_right)
    }
    out
  })

    
  
  
}