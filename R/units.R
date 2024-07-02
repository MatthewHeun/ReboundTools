#' Units for variables
#'
#' Determine the unit for a variable in the rebound framework.
#' 
#' Information about the particular case is required, 
#' thus `service_unit` and `energy_engr_unit` are arguments.
#' 
#' To surround the result with a string, 
#' both `surround_left` and `surround_right` must be specified.
#' For example setting `surround_left = "["` and `surround_right = "]"` (the defaults)
#' gives "\[unit\]".
#' 
#' This function is vectorized.
#'  
#' @param .var_name The variable name for which the unit is to be determined.
#' @param service_unit The energy service unit for this case (a string).
#' @param energy_engr_unit The engineering unit for energy (a string).
#' @param escape_latex A boolean that tells whether to encode the result as LaTeX output.
#' @param surround_left,surround_right,leading_delta_pattern,energy_si,time_unit,inverse_time_unit,currency,currency_latex,unitless,unitless_latex See `ReboundTools::rebound_units`.
#' @param service_unit_name,energy_engr_unit_name,energy_converter,p_E_engr_units,k,I_E,r See `ReboundTools::eeu_base_params`.
#' @param p_E,p_s,q_dot_s,eta_engr_units,efficiency,sigma,rho,income_rate,income,freed_cash_rate,freed_cash,energy 
#'        These arguments describe string prefixes that identify variables for unit determination. 
#' @param S_dot_dev,G_dot,f_Cs See `ReboundTools::star_vars`. 
#'        These arguments describe string patterns that identify variables for unit determination. 
#' @param elasticities,time,cost_rate,cost,energy_rate,rebound,R_alpha,R_omega Other arguments to identify variable names.
#' @param trim_stage_pattern A regex that trims the rebound stage from a variable name. 
#'        Default is "_\[^_\]*$", which trims all characters beyond the last "_" in a string.
#'        
#' @return A string for the units for `.var_name`.
#' 
#' @export
#'
#' @examples
#' rebound_var_units("eta_engr_units_orig", service_unit = "lm-hr", 
#'                   energy_engr_unit = "kW-hr") 
#' rebound_var_units("p_s", service_unit = "lm-hr", energy_engr_unit = "kW-hr") 
#' rebound_var_units("p_s_orig", service_unit = "lm-hr", energy_engr_unit = "kW-hr") 
#' rebound_var_units("Delta_C_dot_o_hat", service_unit = "lm-hr",
#'                   energy_engr_unit = "kW-hr") 
#' rebound_var_units(c("eta_engr_units_orig", "Delta_C_dot_o_hat"), 
#'                     service_unit = c("lm-hr", "lm-hr"), 
#'                     energy_engr_unit = c(energy_engr_unit = "kW-hr"))
rebound_var_units <- function(.var_name, service_unit, energy_engr_unit, 
                              escape_latex = FALSE,
                              surround_left = ReboundTools::rebound_units$surround_left, 
                              surround_right = ReboundTools::rebound_units$surround_right,
                              leading_delta_pattern = ReboundTools::rebound_units$leading_delta_pattern,
                              energy_si = ReboundTools::rebound_units$energy_si,
                              time_unit = ReboundTools::rebound_units$time_unit,
                              inverse_time_unit = ReboundTools::rebound_units$inverse_time_unit,
                              currency = ReboundTools::rebound_units$currency_unit, 
                              currency_latex = ReboundTools::rebound_units$currency_unit_latex,
                              unitless = ReboundTools::rebound_units$unitless, 
                              unitless_latex = ReboundTools::rebound_units$unitless_latex,
                              
                              service_unit_name = ReboundTools::eeu_base_params$service_unit,
                              energy_engr_unit_name = ReboundTools::eeu_base_params$energy_engr_unit,
                              energy_converter = ReboundTools::eeu_base_params$MJ_engr_unit,
                              p_E_engr_units = ReboundTools::eeu_base_params$p_E_engr_units,
                              k = ReboundTools::eeu_base_params$k,  
                              I_E = ReboundTools::eeu_base_params$I_E,
                              r = ReboundTools::eeu_base_params$r,
                              
                              trim_stage_pattern = "_[^_]*$",
                              p_E = ReboundTools::orig_vars$p_E,
                              p_s = sub(x = ReboundTools::orig_vars$p_s_orig, pattern = trim_stage_pattern, replacement = ""),
                              q_dot_s = sub(x = ReboundTools::orig_vars$q_dot_s_orig, pattern = trim_stage_pattern, replacement = ""),
                              eta_engr_units = sub(x = ReboundTools::orig_vars$eta_engr_units_orig, pattern = trim_stage_pattern, replacement = ""),
                              efficiency = sub(x = ReboundTools::orig_vars$eta_orig, pattern = trim_stage_pattern, replacement = ""),
                              sigma = ReboundTools::orig_vars$sigma,
                              rho = ReboundTools::orig_vars$rho,
                              income_rate = sub(x = ReboundTools::orig_vars$M_dot_orig, pattern = trim_stage_pattern, replacement = ""), 
                              income = sub(x = income_rate, pattern = trim_stage_pattern, replacement = ""), 
                              freed_cash_rate = sub(x = ReboundTools::orig_vars$N_dot_orig, pattern = trim_stage_pattern, replacement = ""), 
                              freed_cash = sub(x = freed_cash_rate, pattern = trim_stage_pattern, replacement = ""),
                              energy = sub(x = energy_rate, pattern = trim_stage_pattern, replacement = ""),
                              
                              S_dot_dev = ReboundTools::star_vars$S_dot_dev,
                              G_dot = ReboundTools::star_vars$G_dot,
                              f_Cs = sub(x = ReboundTools::orig_vars$f_Cs_orig, pattern = trim_stage_pattern, replacement = ""), 
                              
                              
                              elasticities = "e",
                              time = "t_", 
                              cost_rate = "C_dot",
                              cost = "C_",
                              energy_rate = "E_dot", 
                              rebound = "Re_", 
                              R_alpha = "R_alpha", 
                              R_omega = "R_omega"
                              
) {
  
  if (escape_latex) {
    currency <- currency_latex
    unitless <- unitless_latex
  }
  
  Map(.var_name, service_unit, energy_engr_unit, f = function(v, su, eu) {

    # Get rid of leading "Delta_", if it exists.
    v <- sub(pattern = leading_delta_pattern, replacement = "", v)
    
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
    
    else if (startsWith(v, cost_rate) | 
             startsWith(v, income_rate) | 
             startsWith(v, freed_cash_rate) | 
             startsWith(v, G_dot)) {
      out <- paste0(currency, "/", time_unit)
    } else if (startsWith(v, cost) | 
               startsWith(v, income) | 
               startsWith(v, freed_cash)) {
      out <- currency
    } 
    
    # Energy rate and energy
    
    else if (startsWith(v, energy_rate) | startsWith(v, S_dot_dev)) {
      out <- paste0(energy_si, "/", time_unit)
    } else if (startsWith(v, energy)) {
      out <- energy_si
    }
    
    # Efficiency
    
    else if (startsWith(v, eta_engr_units)) {
      out <- paste0(su, "/", eu)
    }
    else if (startsWith(v, efficiency)) {
      out <- paste0(su, "/", energy_si)
    }
    
    # k or elasticities (both are unitless)
    # Needs to be after efficiencies to distinguish between "eta" and "e".
    
    else if (startsWith(v, k) | startsWith(v, elasticities) | startsWith(v, sigma) | startsWith(v, rho)) {
      out <- unitless
    }
    
    # Rebound
    
    else if (startsWith(v, rebound) | startsWith(v, f_Cs)) {
      out <- unitless
    }
    
    # Real interest rates
    
    else if (v == r) {
      out <- inverse_time_unit
    }
    
    # R_alpha and R_omega values
    
    else if (startsWith(v, R_alpha) | startsWith(v, R_omega)) {
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

