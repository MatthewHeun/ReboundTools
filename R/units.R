#' Units for variables
#'
#'
#' @param .var_name 
#' @param service_unit 
#' @param energy_engr_unit 
#' @param escape_latex 
#' @param surround_left 
#' @param surround_right 
#' @param leading_delta 
#' @param energy_converter 
#' @param energy_si 
#' @param time_unit 
#' @param currency 
#' @param currency_latex 
#' @param unitless 
#' @param unitless_latex 
#' @param p_E_engr_units 
#' @param p_E 
#' @param p_s 
#' @param q_dot_s 
#' @param efficiency_engr_units 
#' @param efficiency 
#' @param k 
#' @param I_E 
#' @param elasticities 
#' @param sigma 
#' @param time 
#' @param cost_rate 
#' @param cost 
#' @param income_rate 
#' @param income 
#' @param freed_cash_rate 
#' @param freed_cash 
#' @param energy_rate 
#' @param energy 
#' @param S_dot_dev 
#' @param G_dot 
#' @param rebound 
#' @param f_Cs
#'
#' @return A string for the units for `.var_name`.
#' 
#' @export
#'
#' @examples
#' units("eta_engr_units_orig", service_unit = "lm-hr", energy_engr_unit = "kW-hr") 
#' units("p_s", service_unit = "lm-hr", energy_engr_unit = "kW-hr") 
#' units("p_s_orig", service_unit = "lm-hr", energy_engr_unit = "kW-hr") 
#' units("Delta_C_dot_o_hat", service_unit = "lm-hr", energy_engr_unit = "kW-hr") 
units <- function(.var_name, service_unit, energy_engr_unit, 
                  escape_latex = FALSE,
                  surround_left = "[", 
                  surround_right = "]",
                  leading_delta = "^Delta_", 
                  service_unit_name = "service_unit",
                  energy_engr_unit_name = "energy_engr_unit",
                  energy_converter = "MJ/energy_engr_unit",
                  energy_si = "MJ",
                  time_unit = "year",
                  currency = "$", 
                  currency_latex = "\\$",
                  unitless = "-", 
                  unitless_latex = "--",
                  p_E_engr_units = "p_E_engr_units",
                  p_E = "p_E",
                  p_s = "p_s", 
                  q_dot_s = "q_dot_s",
                  efficiency_engr_units = "eta_engr_units",
                  efficiency = "eta",
                  k = "k", 
                  I_E = "I_E",
                  elasticities = "e",
                  sigma = "sigma",
                  time = "t_", 
                  cost_rate = "C_dot",
                  cost = "C_",
                  income_rate = "M_dot", 
                  income = "M", 
                  freed_cash_rate = "N_dot", 
                  freed_cash = "N",
                  energy_rate = "E_dot", 
                  energy = "E",
                  S_dot_dev = "S_dot_dev",
                  G_dot = "G_dot",
                  rebound = "Re_", 
                  f_Cs = "f_Cs"
                  ) {
  
  if (escape_latex) {
    currency <- currency_latex
    unitless <- unitless_latex
  }
  
  Map(.var_name, service_unit, energy_engr_unit, f = function(v, su, eu) {

    # Get rid of leading "Delta_", if it exists.
    v <- sub(pattern = leading_delta, replacement = "", v)
    
    # Service unit and energy_engr_unit
    if (startsWith(v, service_unit_name)) {
      out <- su
    } else if (startsWith(v, energy_engr_unit_name)) {
      out <- eu
    }
    
    # Price of energy
    
    else if (startsWith(v, p_E_engr_units)) {
      out <- paste0(currency, "/", eu)
    } else if (startsWith(v, p_E)) {
      out <- paste0(currency, "/", energy_si)
    } 
    
    # Energy service price
    
    else if (startsWith(v, p_s)) {
      out <- paste0(currency, "/", su)
    }
    
    # Energy service rate
    
    else if (startsWith(v, q_dot_s)) {
      out <- paste0(su, "/", time_unit)
    }
    
    # Energy intensity of the economy
    
    else if (startsWith(v, I_E)) {
      out <- paste0(energy_si, "/", currency)
    } 
    
    # Energy conversion factor
    
    else if (startsWith(v, energy_converter)) {
      out <- paste0(energy_si, "/", eu)
    }
    
    # Time variables
    
    else if (startsWith(v, time)) {
      out <- time_unit
    }
    
    # Cost rate and cost
    
    else if (startsWith(v, cost_rate) | startsWith(v, income_rate) | startsWith(v, freed_cash_rate) | startsWith(v, G_dot)) {
      out <- paste0(currency, "/", time_unit)
    } else if (startsWith(v, cost) | startsWith(v, income) | startsWith(v, freed_cash)) {
      out <- currency
    } 
    
    # Energy rate and energy
    
    else if (startsWith(v, energy_rate) | startsWith(v, S_dot_dev)) {
      out <- paste0(energy_si, "/", time_unit)
    } else if (startsWith(v, energy)) {
      out <- energy_si
    }
    
    # Efficiency
    
    else if (startsWith(v, efficiency_engr_units)) {
      out <- paste0(su, "/", eu)
    }
    else if (startsWith(v, efficiency)) {
      out <- paste0(su, "/", energy_si)
    }
    
    # k or elasticities (both are unitless)
    # Needs to be after efficiencies to distinguish between "eta" and "e".
    
    else if (startsWith(v, k) | startsWith(v, elasticities) | startsWith(v, sigma)) {
      out <- unitless
    }
    
    # Rebound
    
    else if (startsWith(v, rebound) | startsWith(v, f_Cs)) {
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
  }) %>% 
    unlist()
  
  
  
  
}

