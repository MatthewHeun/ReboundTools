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
                  escape_latex = FALSE,
                  surround_left = "[", 
                  surround_right = "]",
                  leading_delta = "^Delta_", 
                  energy_si = "MJ",
                  time_unit = "year",
                  currency = "$", 
                  currency_latex = "\\$",
                  unitless = "-", 
                  unitless_latex = "--",
                  p_E_engr_units = "p_E_engr_units",
                  p_E = "p_E",
                  efficiency_engr_units = "eta_engr_units",
                  efficiency = "eta",
                  k = "k", 
                  I_E = "I_E",
                  elasticities = "e_",
                  time = "t_", 
                  cost_rate = "C_dot_",
                  cost = "C_",
                  income_rate = "M_dot_", 
                  income = "M_", 
                  freed_cash_rate = "N_dot_", 
                  freed_cash = "N_",
                  energy_rate = "E_dot", 
                  energy = "E",
                  rebound = "Re_"
                  ) {
  
  if (escape_latex) {
    currency <- currency_latex
    unitless <- unitless_latex
  }
  
  sapply(.var_name, function(v){
    
    # Get rid of leading "Delta_", if it exists.
    v <- sub(pattern = leading_delta, replacement = "", v)
    
    # Price of energy
    
    if (startsWith(v, p_E_engr_units)) {
      out <- paste0(currency, "/", energy_engr_unit)
    } else if (startsWith(v, p_E)) {
      out <- paste0(currency, "/", energy_si)
    } 
    
    # Energy intensity of the economy
    
    else if (startsWith(v, I_E)) {
      out <- paste0(energy_si, "/", currency)
    } 
    
    # k or elasticities (both are unitless)
    
    else if (startsWith(v, k) | startsWith(v, elasticities)) {
      out <- unitless
    }
    
    # Time variables
    else if (startsWith(v, time)) {
      out <- time_unit
    }
    
    # Cost rate and cost
    
    else if (startsWith(v, cost_rate) | startsWith(v, income_rate) | startsWith(v, freed_cash_rate)) {
      out <- paste0(currency, "/", time_unit)
    } else if (startsWith(v, cost) | startsWith(v, income) | startsWith(v, freed_cash)) {
      out <- currency
    } 
    
    # Energy rate and energy
    
    else if (startsWith(v, energy_rate)) {
      out <- paste0(energy_si, "/", time_unit)
    } else if (startsWith(v, energy)) {
      out <- energy_si
    }
    
    # Efficiency
    
    else if (startsWith(v, efficiency_engr_units)) {
      out <- paste0(service_unit, "/", energy_engr_unit)
    }
    else if (startsWith(v, efficiency)) {
      out <- paste0(service_unit, "/", energy_si)
    }
    
    # Rebound
    
    else if (startsWith(v, rebound)) {
      out <- unitless
    }
    
    # No valid variable found.
    
    else {
      out <- "unknown"
    }
    
    # Deal with surrounding characters, if requested
    if (!is.null(surround_left) & !is.null(surround_right)) {
      out <- paste0(surround_left, out, surround_right)
    }
    
    return(out)
  })
  
  
  
  
}