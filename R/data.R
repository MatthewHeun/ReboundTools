#' EEU data table metadata
#'
#' An EEU (energy efficiency upgrade) data table
#' contains user-supplied information needed to estimate energy rebound.
#' The `ReboundTools` package defines a standard nomenclature
#' for the columns in EEU data tables.
#' This object documents that nomenclature.
#' 
#' @format A string list with `r length(eeu_data_table)` entries.
#' \describe{
#' \item{eeu_data_sheet}{The name of the sheet in an Excel file containing EEU data.}
#' }
#' 
#' @examples
#' eeu_data_table
"eeu_data_table"


#' Information for rebound units
#' 
#' A list of names and constants for rebound units. 
#' 
#' @format A list with `r length(rebound_units)` entries.
#' \describe{
#' \item{energy_si}{The SI energy unit ("MJ").}
#' \item{time_unit}{The time unit ("yr").}
#' \item{inverse_time_unit}{Inverse time unit ("1/yr").}
#' \item{currency_unit}{The default currency unit ("$").}
#' \item{currency_unit_latex}{The currency unit in LaTeX format ("$" with escaping backslashes).}
#' \item{unitless}{The identifier for unitless variables ("-").}
#' \item{unitless_latex}{The identifier for unitless variables in LaTeX format ("--").}
#' \item{leading_delta_pattern}{A regex pattern that identifies a leading Delta ("^Delta_").}
#' \item{surround_left}{The left delimiter for units ("\[").}
#' \item{surround_right}{The right delimiter for units ("\]").}
#' }
#' 
#' @examples
#' rebound_units
"rebound_units"


#' LaTeX key analysis variables
#' 
#' A data frame of key analysis variables in LaTeX formatting.
#' 
#' @format A data frame with `r length(latex_key_analysis_vars)` columns.
#' \describe{
#' \item{var_name}{The text name for key analysis variables.}
#' \item{latex_var_name}{The LaTeX key analysis variable name.}
#' }
#' 
#' @examples
#' latex_key_analysis_vars
"latex_key_analysis_vars"


#' Key analysis variables 
#' 
#' This is the list of key rebound analysis variables.
#' These variables are differenced later in the analysis.
#' 
#' @format A string list with `r length(key_analysis_vars)` entries.
#' \describe{
#' \item{t_life}{The expected lifetime of the device.}
#' \item{tau_alpha}{A parameter that accounts for discounting of beginning-of-life purchases.}
#' \item{tau_omega}{A parameter that accounts for discounting of end-of-life purchases.}
#' \item{eta_engr_units}{Energy service efficiency, calculated by energy service divided by final energy consumed to provide that service, in engineering units.}
#' \item{eta}{Energy service efficiency, calculated by energy service divided by final energy consumed to provide that service.}
#' \item{p_s}{Energy service price \[service/MJ\], calculated by `p_E/eta`.}
#' \item{q_dot_s}{The rate of energy service consumption \[service/yr\], calculated by `eta*E_dot_s`.}
#' \item{p_E}{The price of energy \[$/MJ\]}
#' \item{E_dot_s}{The rate of final energy consumption by the energy conversion device \[MJ/yr\].}
#' \item{E_dot_emb}{The rate of embodied energy demand by the energy conversion device \[MJ/yr\], calculated by `E_emb/t_life`.}
#' \item{C_dot_s}{The expenditure rate of energy consumption by the device \[$/yr\], calculated by `p_s*q_dot_s`.}
#' \item{C_dot_cap}{The capital expenditure rate of the device without discounting \[$/yr\], calculated by `C_cap/t_life`.}
#' \item{C_dot_om}{The operations and maintenance expenditure rate of the device \[$/yr\].}
#' \item{C_d}{The disposal cost for the device \[$/yr\].}
#' \item{C_dot_d}{The disposal cost rate of the device without discounting \[$/yr\].}
#' \item{tau_omega_C_dot_d}{A parameter that accounts for discounting of disposal costs.}
#' \item{C_dot_omd}{The operations, maintenance, and disposal expenditure rate of the device \[$/yr\].}
#' \item{C_dot_g}{The other goods consumption rate \[$/yr\], calculated, initially, as a residual of the budget constraint.}
#' \item{N_dot}{Net income \[$/yr\].}
#' \item{M_dot}{Real income \[$/yr\].}
#' }
#' 
#' @examples
#' key_analysis_vars
"key_analysis_vars"


#' Rebound stages
#' 
#' This is the list of rebound stages.
#' 
#' @format A string list with `r length(rebound_stages)` entries.
#' \describe{
#' \item{orig}{The original (pre-EEU) stage.}
#' \item{star}{The upgraded condition (post-EEU), before any behavior changes.}
#' \item{hat}{After the substitution effect but before the income effect.}
#' \item{bar}{After the income effect but before the macro effect.}
#' \item{tilde}{After the macro effect.}
#' }
#' 
#' @examples
#' rebound_stages
"rebound_stages"


#' LaTeX rebound stages
#' 
#' A data frame of rebound stages in LaTeX formatting.
#' 
#' @format A data frame with `r ncol(latex_rebound_stages)` columns.
#' \describe{
#' \item{stage}{The text name for the stage.}
#' \item{latex_stage_name}{The LaTeX name for the stage.}
#' }
#' 
#' @examples
#' latex_rebound_stages
"latex_rebound_stages"


#' A visibility mask for the `stages_table()`
#' 
#' This data frame contains a column
#' that tells whether values are shown
#' in the [stages_table()].
#' 
#' @format A data frame with three columns, "Variable", "Stage", and "Visible". 
#' \describe{
#' \item{Variable}{The name of the variable in the [stages_table()].}
#' \item{Stage}{The name of the rebound in the [stages_table()].}
#' \item{Visible}{A boolean that tells whether this combination of `Variable` and `Stage` should be visible.}
#' }
#' 
#' @examples
#' stages_table_visibility_mask
"stages_table_visibility_mask"


#' Rebound segment names
#' 
#' A list of rebound segment names used internally to the package.
#' 
#' @format A list frame with `r length(rebound_segments)` entries
#' \describe{
#' \item{dempl}{Direct emplacement effects.}
#' \item{emb}{Embodied energy effects.}
#' \item{cap}{Capital expenditure effects.}
#' \item{md}{Maintenance and disposal effects.}
#' \item{dsub}{Direct substitution effects.}
#' \item{isub}{Indirect substitution effects.}
#' \item{dinc}{Direct income effects.}
#' \item{iinc}{Indirect income effects.}
#' \item{macro}{Macro effects.}
#' }
#' 
#' @examples
#' rebound_segments
"rebound_segments"


#' Difference variables
#' 
#' This is the list of difference variables of the form `Delta_var_stage`, where
#' `var` is the variable name (from `ReboundTools::key_analysis_vars`) and 
#' `stage` is the rebound stage being considered (from `ReboundTools::rebound_stages`).
#' 
#' @format A string list with `r length(Delta_vars)` entries.
#' \describe{
#' \item{Delta_eta_star}{Calculated as `eta_star - eta_orig`.}
#' \item{Delta_p_s_star}{Calculated as `p_s_star - p_s_orig`.}
#' \item{.}{Etc.}
#' \item{.}{Etc.}
#' \item{.}{Etc.}
#' \item{Delta_M_dot_tilde}{Calculated as `M_dot_tilde - M_dot_bar`.}
#' \item{Delta_N_dot_tilde}{Calculated as `N_dot_tilde - N_dot_bar`.}
#' }
#' 
#' @examples
#' Delta_vars
"Delta_vars"


#' EEU base parameters 
#' 
#' This is the list of the required parameters for a rebound analysis. 
#' These parameters must be supplied for a rebound analysis to commence. 
#' The names in this list are the assumed names throughout this package, 
#' but they can always be overridden by function arguments.
#' 
#' In the descriptions for each parameter, typical units are provided for 
#' energy efficiency upgrades for cars and electric lamps.
#' 
#' Note the following meanings for the default parameter names:
#' \describe{
#' \item{orig}{The original device, prior to the EEU.}
#' \item{star}{After the emplacement effect and before the substitution effect.}
#' \item{hat}{After the substitution effect and before the income effect.}
#' \item{bar}{After the income effect and before the macro effect.}
#' \item{tilde}{After the macro effect.}
#' \item{I}{An economic intensity (per $).}
#' \item{p}{Price.}
#' \item{E}{Energy, typically final energy.}
#' \item{s}{Energy service.}
#' \item{o}{Other goods.}
#' \item{eps}{Elasticity.}
#' \item{UC}{Uncompensated.}
#' \item{dot}{Signifies a rate, typically per yr.}
#' \item{q}{Quantity of energy service or other goods consumed.}
#' \item{M}{Income.}
#' \item{C}{Cost.}
#' \item{t}{Expected device lifetime.}
#' }
#' 
#' @format A string list with `r length(eeu_base_params)` entries.
#' \describe{
#' \item{reference}{A string for a reference for this case, e.g. bibliographic entry for a paper in which this case appears.}
#' \item{case}{A string to identify the case being analyzed, e.g., "Lamp".}
#' \item{original}{A string to identify the original device, e.g., "Incandescent".}
#' \item{upgrade}{A string to identify the upgraded device, e.g., "LED".}
#' \item{r}{The discount rate, in units of 1/year. E.g., 3.0% should be entered as 0.03.}
#' \item{service_unit}{A string to identify the unit of the energy service, e.g., "miles" in "miles/gal" or "lm-hr" in "lm-hr/kW-hr".}
#' \item{energy_engr_unit}{A string to identify the energy units of the service, e.g., "gal" in "miles/gal" or "kW-hr" in "lm-hr/kW-hr".}
#' \item{MJ_engr_unit}{A unit conversion factor: the number of MJ per engineering unit for the service efficiency. For example, if the service efficiency is given in miles/gallon, `MJ_engr_unit` should be 126.6 MJ/gallon. This unit conversion number is used in calculating the actual service efficiency.}
#' \item{I_E}{The energy intensity of the economy \[MJ/$\].}
#' \item{k}{The macro effect factor \[--\].}
#' \item{p_E_engr_units}{The price of energy in engineering units, e.g., $/gal or $/kW-hr \[$/energy_engr_unit\].}
#' \item{e_qs_ps_UC_orig}{The original uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of energy service ("qs") consumption (own-price elasticity) \[--\].}
#' \item{e_qs_M}{The income ("M") elasticity ("e") of energy service ("qs") consumption \[--\].}
#' \item{e_qg_M}{The income ("M") elasticity ("e") of other goods ("qo") consumption \[--\].}
#' }
#' 
#' @examples
#' eeu_base_params
"eeu_base_params"


#' EEU orig data 
#' 
#' This is the list of the derived variables at the original (pre-EEU) stage of a rebound analysis. 
#' 
#' @format A string list with `r length(orig_vars)` entries.
#' \describe{
#' \item{tau_alpha_orig}{The original discount multiplier for beginning-of-life expenses, such as capital cost.}
#' \item{tau_omega_orig}{The original discount multiplier for end-of-life expenses, such as disposal cost.}
#' \item{p_E}{The price of energy \[$/MJ\], calculated by `p_E_engr_units / MJ_engr_unit`.}
#' \item{p_E_orig}{The price of energy \[$/MJ\].}
#' \item{q_dot_s_orig}{The original (pre-EEU) consumption rate of the energy service. Example units are \[miles/yr\] \[lumen-hours/yr\].}
#' \item{C_cap_orig}{The net capital expenditure of the original device: the sum of purchase price and financing costs less rebates and resale value at end of ownership \[$\].}
#' \item{M_dot_orig}{The disposable income rate, exclusive of taxes and savings \[$/yr\].}
#' \item{eta_engr_units_orig}{The original (pre-EEU) energy service efficiency.  This number should have engineering units in the denominator, e.g., \[miles/gallon\] \[lumens/kW\]. Note that the denominator unit of `eta_engr_units_orig` is assumed to be the same as the denominator unit of `MJ_engr_unit`.}
#' \item{eta_orig}{Energy service efficiency of the original (pre-EEU) device on a per-MJ basis \[service/MJ\], calculated by `eta_engr_units_orig / MJ_engr_unit`.}
#' \item{E_dot_s_orig}{The final energy consumption rate of the original (pre-EEU) device \[MJ/yr\], calculated by `q_dot_s_orig / eta_orig`.}
#' \item{C_dot_cap_orig}{The capital expenditure rate of the device without discounting \[$/yr\], calculated by `C_cap/t_own`.}
#' \item{tau_alpha_C_dot_cap_orig}{The product of tau_alpha and the undiscounted capital cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_om_orig}{The operations and maintenance expenditure rate of the device \[$/yr\].}
#' \item{C_d_orig}{The disposal cost for the device \[$/yr\].}
#' \item{C_dot_d_orig}{The disposal cost rate of the device without discounting \[$/yr\].}
#' \item{tau_omega_C_dot_d_orig}{The product of tau_omega and the undiscounted disposal cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_omd_orig}{The operations, maintenance, and disposal expenditure rate of the device \[$/yr\].}
#' \item{p_s_orig}{The original (pre-EEU) energy service price \[$/service\], calculated by `p_E / eta_orig`.}
#' \item{C_dot_s_orig}{The original (pre-EEU) rate of energy expenditures for the device \[$/yr\], calculated by `p_E * E_dot_s_orig`.}
#' \item{C_dot_g_orig}{The original (pre-EEU) rate of expenditure on other goods \[$/yr\], calculated by `M_dot_orig - C_dot_s_orig - C_dot_cap_orig - C_dot_md_orig`.}
#' \item{f_Cs_orig}{The original (pre-EEU) fraction of the energy and other budget spent on the energy service \[--\], calculated by `C_dot_s_orig / (C_dot_s_orig + C_dot_g_orig)`.}
#' \item{e_qs_ps_C_orig}{The original compensated energy service price ("ps") elasticity ("e") of energy service ("qs") consumption \[--\], calculated by `e_qs_ps_UC_orig + f_Cs_orig*e_qs_M`.}
#' \item{e_qg_ps_C_orig}{The original compensated energy service price ("ps") elasticity ("e") of other goods ("qo") consumption \[--\], calculated by `f_Cs_orig*(f_Cs_orig + e_qs_ps_UC_orig) / (f_Cs_orig - 1)`.}
#' \item{e_qg_ps_UC_orig}{The original uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of other goods ("qo") consumption (cross-price elasticity) \[--\].}
#' \item{sigma}{The elasticity of substitution between energy service consumption and other goods consumption \[--\].}
#' \item{rho}{The exponent in the CES utility model, defined as rho = 1/sigma - 1 \[--\].}
#' \item{E_emb_orig}{The embodied energy of the original (pre-EEU) device \[MJ\].}
#' \item{t_life_orig}{The expected lifetime of the original (pre-EEU) device \[yr\].}
#' \item{E_dot_emb_orig}{The original (pre-EEU) rate of embodied energy demand\[MJ/yr\], calculated by `E_dot_emb / t_orig`.}
#' \item{N_dot_orig}{The original (pre-EEU) freed cash rate \[$/yr\], exactly `0`.}
#' }
#' @examples
#' orig_vars
"orig_vars"


#' EEU star data 
#' 
#' This is the list of the derived variables at the star (post-emplacement) stage of a rebound analysis. 
#' 
#' @format A string list with `r length(star_vars)` entries.
#' \describe{
#' \item{tau_alpha_star}{The post-emplacement discount multiplier for beginning-of-life expenses, such as capital cost.}
#' \item{tau_omega_star}{The post-emplacement discount multiplier for end-of-life expenses, such as disposal cost.}
#' \item{C_cap_star}{The net capital expenditure of the upgraded device: the sum of purchase price and financing costs less rebates and resale value at end of ownership \[$\].}
#' \item{C_dot_md_star}{The upgraded (post-EEU) maintenance and disposal expenditure rate \[$/yr\].}
#' \item{E_emb_star}{The embodied energy of the upgraded (post-EEU) device \[MJ\].}
#' \item{t_life_star}{The expected lifetime of the upgraded (post-EEU) device \[yr\].}
#' \item{eta_engr_units_star}{The upgraded (post-EEU) energy service efficiency. This number should have engineering units in the denominator, e.g., \[miles/gallon\] \[lumens/kW\]. Note that the denominator unit of `eta_engr_units_orig` is assumed to be the same as the denominator unit of `MJ_engr_unit`.}
#' \item{eta_star}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], calculated by `eta_tilde_engr_units / MJ_engr_unit`.}
#' \item{eta_ratio}{The energy service efficiency ratio \[--\], calculated by `eta_star/eta_orig`.}
#' \item{S_dot_dev}{The expected device-level energy savings rate \[MJ/yr\], calculated by `(eta_ratio - 1) * (1/eta_ratio) * E_dot_s_orig`.}
#' \item{G_dot}{The expected device-level energy gross cost savings rate \[MJ/yr\], calculated by `p_E * S_dot_dev`.}
#' \item{p_s_star}{The upgraded (post-EEU) energy service price \[$/service\], calculated by `p_E / eta_star = p_E / eta_tilde`.}
#' \item{q_dot_s_star}{The upgraded (post-EEU) energy service consumption rate \[service/yr\], same as `q_dot_s_orig`.}
#' \item{C_dot_cap_star}{The capital expenditure rate of the device without discounting \[$/yr\], calculated by `C_cap/t_life`.}
#' \item{tau_alpha_C_dot_cap_star}{The product of tau_alpha and the undiscounted capital cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_om_star}{The operations and maintenance expenditure rate of the device \[$/yr\].}
#' \item{C_d_star}{The disposal cost for the device \[$/yr\].}
#' \item{C_dot_d_star}{The disposal cost rate of the device without discounting \[$/yr\].}
#' \item{tau_omega_C_dot_d_star}{The product of tau_omega and the undiscounted disposal cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_omd_star}{The operations, maintenance, and disposal expenditure rate of the device \[$/yr\].}
#' \item{E_dot_emb_star}{The upgraded (post-EEU) embodied energy rate \[MJ/yr\], calculated by `E_emb_star / t_star`.}
#' \item{C_dot_s_star}{The upgraded (post-EEU) energy expenditure rate \[$/yr\], calculated by `p_s_star * q_dot_s_star`.}
#' \item{M_dot_star}{The disposable income rate, exclusive of taxes and savings \[$/yr\], exactly `M_dot_orig`.}
#' \item{N_dot_star}{The freed cash rate \[$/yr\], calculated by `G_dot - (C_dot_cap_star - C_dot_cap_orig) - (C_dot_md_star - C_dot_md_orig)`.}
#' \item{C_dot_g_star}{The upgraded (post-EEU) other goods expenditure rate \[$/yr\], exactly `C_dot_g_orig`.}
#' \item{f_Cs_star}{The upgraded (post-EEU) fraction of the energy and other budget spent on the energy service \[--\], calculated by `C_dot_s_star / (C_dot_s_star + C_dot_g_star)`.}
#' \item{e_qs_ps_C_star}{The upgraded (post-EEU) compensated energy service price ("ps") elasticity ("e") of energy service ("qs") consumption \[--\], calculated by `e_qs_ps_UC_orig + f_Cs_orig*e_qs_M`.}
#' \item{e_qg_ps_C_star}{The upgraded (post-EEU) compensated energy service price ("ps") elasticity ("e") of other goods ("qo") consumption \[--\], calculated by `f_Cs_orig*(f_Cs_orig + e_qs_ps_UC_orig) / (f_Cs_orig - 1)`.}
#' \item{e_qs_ps_UC_star}{The upgraded (post-EEU) uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of energy service ("qs") consumption (own-price elasticity) \[--\].}
#' \item{e_qg_ps_UC_star}{The upgraded (post-EEU) uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of other goods ("qo") consumption (cross-price elasticity) \[--\].}
#' \item{p_E_star}{The price of energy \[$/MJ\],}
#' \item{E_dot_s_star}{The upgraded (post-EEU) energy consumption rate \[MJ/yr\], calculated by `q_dot_s_star / eta_star`.}
#' }
#' @examples
#' star_vars
"star_vars"


#' EEU hat data 
#' 
#' This is the list of the derived variables at the hat stage (after substitution effect) of a rebound analysis.
#' 
#' @format A string list with `r length(hat_vars)` entries.
#' \describe{
#' \item{tau_alpha_hat}{The post-substitution effect discount multiplier for beginning-of-life expenses, such as capital cost. Same as `tau_alpha_star`.}
#' \item{tau_omega_hat}{The post-substitution effect discount multiplier for end-of-life expenses, such as disposal cost. Same as `tau_omega_star`.}
#' \item{eta_hat}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], exactly `eta_star`.}
#' \item{p_s_hat}{The energy service price after the substitution effect \[$/service\], exactly `p_s_star`.}
#' \item{C_dot_cap_hat}{The capital expenditure rate of the device without discounting \[$/yr\], calculated by `C_cap/t_life`.}
#' \item{tau_alpha_C_dot_cap_hat}{The product of tau_alpha and the undiscounted capital cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_om_hat}{The operations and maintenance expenditure rate of the device \[$/yr\].}
#' \item{C_d_hat}{The disposal cost for the device \[$/yr\].}
#' \item{C_dot_d_hat}{The disposal cost rate of the device without discounting \[$/yr\].}
#' \item{tau_omega_C_dot_d_hat}{The product of tau_omega and the undiscounted disposal cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_omd_hat}{The operations, maintenance, and disposal expenditure rate of the device \[$/yr\].}
#' \item{E_dot_emb_hat}{The embodied energy rate after the substitution effect \[MJ/yr\], exactly `E_dot_emb_star`.}
#' \item{M_dot_hat}{Real income after the substitution effect \[MJ/yr\], exactly `M_dot_star`.}
#' \item{q_dot_s_hat}{The rate of energy service consumption after the substitution effect\ [service/yr\], calculated by `q_dot_s_star * eta_ratio^(-e_qs_ps_C)`.}
#' \item{p_E_hat}{The price of energy \[$/MJ\].}
#' \item{E_dot_s_hat}{The rate of energy consumption after the substitution effect\ [service/yr\], calculated by `q_dot_s_hat / eta_hat`.}
#' \item{C_dot_g_hat}{The rate of other goods expenditures after the substitution effect \[$/yr\], calculated by `C_dot_g_star * eta_ratio^(-e_qo_ps_C)`.}
#' \item{f_Cs_hat}{The post-substitution effect fraction of the energy and other budget spent on the energy service \[--\], calculated by `C_dot_s_star / (C_dot_s_star + C_dot_g_star)`.}
#' \item{e_qs_ps_C_hat}{The post-substitution effect compensated energy service price ("ps") elasticity ("e") of energy service ("qs") consumption \[--\], calculated by `e_qs_ps_UC_orig + f_Cs_orig*e_qs_M`.}
#' \item{e_qg_ps_C_hat}{The post-substitution effect compensated energy service price ("ps") elasticity ("e") of other goods ("qo") consumption \[--\], calculated by `f_Cs_orig*(f_Cs_orig + e_qs_ps_UC_orig) / (f_Cs_orig - 1)`.}
#' \item{e_qs_ps_UC_hat}{The post-substitution effect uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of energy service ("qs") consumption (own-price elasticity) \[--\].}
#' \item{e_qg_ps_UC_hat}{The post-substitution effect uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of other goods ("qo") consumption (cross-price elasticity) \[--\].}
#' \item{N_dot_hat}{The freed cash rate \[$/yr\], calculated by `G_dot - (C_dot_cap_star - C_dot_cap_orig) - (C_dot_md_star - C_dot_md_orig)`.}
#' \item{M_dot_hat_prime}{Modified `M_dot` for the income effect \[$/yr\], calculated by `M_dot_hat - C_dot_cap_orig - C_dot_md_orig - G_dot + p_E*(E_dot_s_hat - E_dot_s_star) + (C_dot_g_hat - C_dot_g_star)`}
#' }
#' @examples
#' hat_vars
"hat_vars"


#' EEU bar data 
#' 
#' This is the list of the derived variables at the bar stage (after income effect) of a rebound analysis.
#' 
#' @format A string list with `r length(bar_vars)` entries.
#' \describe{
#' \item{tau_alpha_bar}{The post-income effect discount multiplier for beginning-of-life expenses, such as capital cost. Same as `tau_alpha_hat`.}
#' \item{tau_omega_bar}{The post-income effect discount multiplier for end-of-life expenses, such as disposal cost. Same as `tau_omega_hat`.}
#' \item{eta_bar}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], exactly `eta_hat`.}
#' \item{p_s_bar}{The energy service price after the income effect \[$/service\], exactly `p_s_hat`.}
#' \item{C_dot_cap_bar}{The capital expenditure rate of the device without discounting \[$/yr\], calculated by `C_cap/t_life`.}
#' \item{tau_alpha_C_dot_cap_bar}{The product of tau_alpha and the undiscounted capital cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_om_bar}{The operations and maintenance expenditure rate of the device \[$/yr\].}
#' \item{C_d_bar}{The disposal cost for the device \[$/yr\].}
#' \item{C_dot_d_bar}{The disposal cost rate of the device without discounting \[$/yr\].}
#' \item{tau_omega_C_dot_d_bar}{The product of tau_omega and the undiscounted disposal cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_omd_bar}{The operations, maintenance, and disposal expenditure rate of the device \[$/yr\].}
#' \item{E_dot_emb_bar}{The embodied energy rate after the income effect \[MJ/yr\], exactly `E_dot_emb_hat`.}
#' \item{M_dot_bar}{Real income after the income effect \[MJ/yr\], exactly `M_dot_hat`.}
#' \item{q_dot_s_bar}{The rate of energy service consumption after the income effect\ [service/yr\], calculated by `(1 + N_dot_hat/M_dot_hat_prime)^(e_qs_M)`.}
#' \item{C_dot_g_bar}{The rate of other goods expenditures after the income effect \[$/yr\], calculated by `(1 + N_dot_hat/M_dot_hat_prime)^(e_qg_M)`.}
#' \item{f_Cs_bar}{The post-income effect fraction of the energy and other budget spent on the energy service \[--\], calculated by `C_dot_s_star / (C_dot_s_star + C_dot_g_star)`.}
#' \item{e_qs_ps_C_bar}{The post-income effect compensated energy service price ("ps") elasticity ("e") of energy service ("qs") consumption \[--\], calculated by `e_qs_ps_UC_orig + f_Cs_orig*e_qs_M`.}
#' \item{e_qg_ps_C_bar}{The post-income effect compensated energy service price ("ps") elasticity ("e") of other goods ("qo") consumption \[--\], calculated by `f_Cs_orig*(f_Cs_orig + e_qs_ps_UC_orig) / (f_Cs_orig - 1)`.}
#' \item{e_qs_ps_UC_bar}{The post-income effect uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of energy service ("qs") consumption (own-price elasticity) \[--\].}
#' \item{e_qg_ps_UC_bar}{The post-income effect uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of other goods ("qo") consumption (cross-price elasticity) \[--\].}
#' \item{p_E_bar}{The price of energy \[$/MJ\],}
#' \item{N_dot_bar}{The freed cash rate after the income effect \[$/yr\], exactly `0`.}
#' }
#' @examples
#' bar_vars
"bar_vars"


#' EEU tilde data 
#' 
#' This is the list of the derived variables at the tilde stage (after macro effect) of a rebound analysis.
#' 
#' @format A string list with `r length(tilde_vars)` entries.
#' \describe{
#' \item{tau_alpha_bar}{The post-macro effect discount multiplier for beginning-of-life expenses, such as capital cost. Same as `tau_alpha_bar`.}
#' \item{tau_omega_bar}{The post-macro effect discount multiplier for end-of-life expenses, such as disposal cost. Same as `tau_omega_bar`.}
#' \item{eta_tilde}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], exactly `eta_bar`.}
#' \item{p_s_tilde}{The energy service price after the macro effect \[$/service\], exactly `p_s_bar`.}
#' \item{C_dot_cap_tilde}{The capital expenditure rate of the device without discounting \[$/yr\], calculated by `C_cap/t_life`.}
#' \item{tau_alpha_C_dot_cap_tilde}{The product of tau_alpha and the undiscounted capital cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_om_tilde}{The operations and maintenance expenditure rate of the device \[$/yr\].}
#' \item{C_d_tilde}{The disposal cost for the device \[$/yr\].}
#' \item{C_dot_d_tilde}{The disposal cost rate of the device without discounting \[$/yr\].}
#' \item{tau_omega_C_dot_d_tilde}{The product of tau_omega and the undiscounted disposal cost rate, itself annualized and discounted in \[$/yr\].}
#' \item{C_dot_omd_tilde}{The operations, maintenance, and disposal expenditure rate of the device \[$/yr\].}
#' \item{E_dot_emb_tilde}{The embodied energy rate after the macro effect \[MJ/yr\], exactly `E_dot_emb_bar`.}
#' \item{M_dot_tilde}{Real income after the macro effect \[MJ/yr\], exactly `M_dot_bar`.}
#' \item{q_dot_s_tilde}{The rate of energy service consumption after the macro effect\ [service/yr\], exactly `q_dot_s_bar`.}
#' \item{C_dot_g_tilde}{The rate of other goods expenditures after the macro effect \[$/yr\], exactly `C_dot_g_bar`.}
#' \item{f_Cs_tilde}{The post-macro effect fraction of the energy and other budget spent on the energy service \[--\], calculated by `C_dot_s_star / (C_dot_s_star + C_dot_g_star)`.}
#' \item{e_qs_ps_C_tilde}{The post-macro effect compensated energy service price ("ps") elasticity ("e") of energy service ("qs") consumption \[--\], calculated by `e_qs_ps_UC_orig + f_Cs_orig*e_qs_M`.}
#' \item{e_qg_ps_C_tilde}{The post-macro effect compensated energy service price ("ps") elasticity ("e") of other goods ("qo") consumption \[--\], calculated by `f_Cs_orig*(f_Cs_orig + e_qs_ps_UC_orig) / (f_Cs_orig - 1)`.}
#' \item{e_qs_ps_UC_tilde}{The post-macro effect uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of energy service ("qs") consumption (own-price elasticity) \[--\].}
#' \item{e_qg_ps_UC_tilde}{The post-macro effect uncompensated ("UC") Marshallian energy service price ("ps") elasticity ("e") of other goods ("qo") consumption (cross-price elasticity) \[--\].}
#' \item{p_E_tilde}{The price of energy \[$/MJ\],}
#' \item{N_dot_tilde}{The freed cash rate after the macro effect \[$/yr\], exactly `0`.}
#' }
#' @examples
#' tilde_vars
"tilde_vars"


#' Rebound terms
#' 
#' This is the list of the rebound terms.
#' 
#' @format A string list with `r length(rebound_terms)` entries.
#' \describe{
#' \item{Re_dempl}{Direct emplacement effect rebound, always 0.}
#' \item{Re_emb}{Indirect embodied energy effect rebound.}
#' \item{Re_cap}{Indirect capital expenditure effect rebound.}
#' \item{Re_om}{Indirect operations and maintenance expenditure effect rebound.}
#' \item{Re_d}{Indirect disposal effect rebound.}
#' \item{Re_omd}{Indirect operations, maintenance, and disposal effect rebound.}
#' \item{Re_empl}{Emplacement effect rebound.}
#' \item{Re_dsub}{Direct substitution effect rebound.}
#' \item{Re_isub}{Indirect substitution effect rebound.}
#' \item{Re_sub}{Substitution effect rebound.}
#' \item{Re_dinc}{Direct income effect rebound.}
#' \item{Re_iinc}{Indirect income effect rebound.}
#' \item{Re_inc}{Income effect rebound.}
#' \item{Re_micro}{Sum of all micro rebound effects.}
#' \item{Re_macro}{Indirect macro effect rebound.}
#' \item{Re_dir}{Sum of all direct rebound effects.}
#' \item{Re_indir}{Sum of all indirect rebound effects.}
#' \item{Re_tot}{Total rebound.}
#' }
#' @examples
#' rebound_terms
"rebound_terms"


#' Aggregate rebound terms
#' 
#' This is the list of the rebound terms that are aggregates of other rebound terms.
#' 
#' @format A string list with `r length(rebound_terms_agg)` entries.
#' \describe{
#' \item{Re_empl}{Emplacement effect rebound.}
#' \item{Re_sub}{Substitution effect rebound.}
#' \item{Re_inc}{Income effect rebound.}
#' \item{Re_dir}{Sum of all direct rebound effects.}
#' \item{Re_indir}{Sum of all indirect rebound effects.}
#' \item{Re_tot}{Total rebound.}
#' }
#' @examples
#' rebound_terms_agg
"rebound_terms_agg"


#' LaTeX version of rebound terms
#'
#' This is the list of the rebound terms in LaTeX form.
#'
#' @format A string list with `r length(latex_rebound_terms)` entries.
#' \describe{
#' \item{Re_dempl}{Direct emplacement effect rebound, always 0.}
#' \item{Re_emb}{Indirect embodied energy effect rebound.}
#' \item{Re_cap}{Indirect capital expenditure effect rebound.}
#' \item{Re_md}{Indirect maintenance and disposal effect rebound.}
#' \item{Re_empl}{Emplacement effect rebound.}
#' \item{Re_dsub}{Direct substitution effect rebound.}
#' \item{Re_isub}{Indirect substitution effect rebound.}
#' \item{Re_sub}{Substitution effect rebound.}
#' \item{Re_dinc}{Direct income effect rebound.}
#' \item{Re_iinc}{Indirect income effect rebound.}
#' \item{Re_inc}{Income effect rebound.}
#' \item{Re_micro}{Sum of all micro rebound effects.}
#' \item{Re_macro}{Indirect macro effect rebound.}
#' \item{Re_dir}{Sum of all direct rebound effects.}
#' \item{Re_indir}{Sum of all indirect rebound effects.}
#' \item{Re_tot}{Total rebound.}
#' }
#' @examples
#' latex_rebound_terms
"latex_rebound_terms"


#' Plane types
#' 
#' The list of plane types in the `ReboundTools` package.
#' 
#' @format A string list with `r length(graph_types)` entries.
#' \describe{
#' \item{energy}{The energy plane graph type.}
#' \item{expenditure}{The expenditure plane graph type.}
#' \item{consumption}{The consumption plane graph type.}
#' }
#' @examples
#' graph_types
"graph_types"


#' Names of graph data frame columns
#' 
#' The list of names of graph data frame columns. 
#' These are default names for columns produced internally.
#' 
#' @format A string list with `r length(graph_df_colnames)` entries.
#' \describe{
#' \item{colour_col}{The name of the column containing line colours.}
#' \item{size_col}{The name of the column containing line sizes (widths).}
#' \item{linetype_col}{The name of the column containing line types.}
#' \item{graph_type_col}{The name of the column containing graph types.}
#' \item{line_name_col}{The name of the column containing names (string identifiers) for lines, segments, and grids.}
#' \item{slope_col}{The name of the column containing line slopes.}
#' \item{intercept_col}{The name of the column containing line intercepts.}
#' \item{x_col}{The name of the column containing starting x values.}
#' \item{y_col}{The name of the column containing starting y values.}
#' \item{xend_col}{The name of the column containing ending x values.}
#' \item{yend_col}{The name of the column containing ending y values.}
#' \item{qs1_qs0_col}{The name of the column containing a q_s/q_s_0 point on this indifference curve.}
#' \item{Cg1_Cg0_col}{The name of the column containing a C_g/C_g_0 point on this indifference curve.}
#' \item{f_Cs_orig_col}{The name of the column containing the original value of f_Cs for this indifference curve.}
#' \item{sigma_col}{The name of the column containing ending sigma values for this indifference curve.}
#' \item{start_point_col}{The name of the boolean column telling whether this row contains a segment that should have a starting point.}
#' \item{end_arrow_col}{The name of the boolean column telling whether this row contains a segment that should have an ending arrow.}
#' \item{Re_names}{The name of the column of rebound names.}
#' \item{Re_values}{The name of the column of rebound values.}
#' \item{y_names_col}{The name of the column of y-axis variable names.}
#' \item{y_vals_col}{The name of the column of y-axis variable names.}
#' }
#' @examples
#' graph_df_colnames
"graph_df_colnames"


#' Path graph parameters
#' 
#' The list of graph parameters for drawing
#' energy path graphs, expenditure path graphs, and consumption path graphs
#' in the `ReboundTools` package.
#' 
#' This list is passed to several graphing functions.
#' Callers may pass a modified version of this list 
#' to change graph appearance.
#' 
#' @format A string list with `r length(path_graph_params)` entries.
#' \describe{
#' 
#' \item{which_points}{A data frame telling which points to include in the graph.}
#' \item{last_point}{Tells whether to show the last point in a path. Overrides `which_points`.}
#' \item{point_shape}{The shape for points between rebound effects. Default is `21`, a filled circle..} 
#' \item{point_size}{The size for points between rebound effects. Default is `1`.}
#' \item{point_stroke}{The size of the line surrounding points between rebound effects. Default is `1`.}
#' \item{which_arrows}{A data frame telling which ending arrows to include in the graph.}
#' \item{last_arrow}{Tells whether to show the last arrow. Overrides `which_arrows`.}
#' \item{arrow_style}{An `arrow` object created by `grid::arrow`.}
#' \item{show_indifference_curves}{A boolean that tells whether to include indifference curves on consumption path graphs. Default is `TRUE`.}
#' \item{dempl_colour}{The colour for direct emplacment lines.}
#' \item{emb_colour}{The colour for embodied energy lines.}
#' \item{cap_colour}{The colour for capital expenditure lines.}
#' \item{md_colour}{The colour for maintenance and disposal lines.}
#' \item{empl_colour}{The colour for emplacement lines.}
#' \item{dsub_colour}{The colour for direct substitution lines.}
#' \item{isub_colour}{The colour for indirect substitution lines.}
#' \item{sub_colour}{The colour for substitution lines.}
#' \item{dinc_colour}{The colour for direct income lines.}
#' \item{iinc_colour}{The colour for indirect income lines.}
#' \item{inc_colour}{The colour for income lines.}
#' \item{macro_colour}{The colour for macro lines.}
#' \item{dir_colour}{The colour for direct lines.}
#' \item{indir_colour}{The colour for macro lines.}
#' \item{tot_colour}{The colour for total rebound lines.}
#' \item{dempl_size}{The size for direct emplacment lines.}
#' \item{emb_size}{The size for embodied energy lines.}
#' \item{cap_size}{The size for capital expenditure lines.}
#' \item{md_size}{The size for maintenance and disposal lines.}
#' \item{empl_size}{The size for emplacement lines.}
#' \item{dsub_size}{The size for direct substitution lines.}
#' \item{isub_size}{The size for indirect substitution lines.}
#' \item{sub_size}{The size for substitution lines.}
#' \item{dinc_size}{The size for direct income lines.}
#' \item{iinc_size}{The size for indirect income lines.}
#' \item{inc_size}{The size for income lines.}
#' \item{macro_size}{The size for macro lines.}
#' \item{dir_size}{The size for direct lines.}
#' \item{indir_size}{The size for indirect lines.}
#' \item{tot_size}{The size for total rebound lines.}
#' \item{dempl_linetyps}{The linetype for direct emplacment lines.}
#' \item{emb_linetype}{The linetype for embodied energy lines.}
#' \item{cap_linetype}{The linetype for capital expenditure lines.}
#' \item{md_linetype}{The linetype for maintenance and disposal lines.}
#' \item{empl_linetype}{The linetype for emplacement lines.}
#' \item{dsub_linetype}{The linetype for direct substitution lines.}
#' \item{isub_linetype}{The linetype for indirect substitution lines.}
#' \item{sub_linetype}{The linetype for substitution lines.}
#' \item{dinc_linetype}{The linetype for direct income lines.}
#' \item{iinc_linetype}{The linetype for indirect income lines.}
#' \item{inc_linetype}{The linetype for income lines.}
#' \item{macro_linetype}{The linetype for macro lines.}
#' \item{dir_linetype}{The linetype for direct lines.}
#' \item{indir_linetype}{The linetype for indirect lines.}
#' \item{tot_linetype}{The linetype for total rebound lines.}
#' \item{lineend}{The line end style.}
#' \item{linejoin}{The line join style.}
#' \item{reverse_path_drawing_order}{Tells whether to reverse the drawing order for paths. The default (`FALSE`)
#'                                   draws emplacement on the bottom, followed by substitution, income, and macro paths.
#'                                   `TRUE` puts macro paths on the bottom, followed by income, substitution, and
#'                                   emplacement paths.
#'                                   Setting `TRUE` produces attractive layering when many paths have arrows, because
#'                                   arrows overlay their following points.}
#' \item{points_atop_paths}{Tells whether to draw points above paths (`TRUE`) or beneath paths (`FALSE`). Default is `TRUE`.}
#' \item{energy_grid_colour}{The colour for energy grid lines.}
#' \item{zero_perc_rebound_grid_colour}{The colour for energy the 0% rebound lines.}
#' \item{hundred_perc_rebound_grid_colour}{The colour for the 100% rebound lines.}
#' \item{energy_rebound_lines_colour}{The colour for energy rebound lines.}
#' \item{expenditure_grid_colour}{The colour for expenditure grid lines.}
#' \item{expenditure_ray_colour}{The colour for expenditure rays.}
#' \item{cons_grid_colour}{The colour for consumption grid lines.}
#' \item{cons_indiff_curve_colour}{The colour for indifference curve lines.}
#' \item{energy_grid_size}{The size for energy grid lines.}
#' \item{zero_perc_rebound_grid_size}{The size for 0% rebound grid lines.}
#' \item{hundred_perc_rebound_grid_size}{The size for the 100% rebound grid lines.}
#' \item{energy_rebound_lines_size}{The size for energy rebound lines.}
#' \item{expenditure_grid_size}{The size for expenditure grid lines.}
#' \item{expenditure_ray_size}{The size for expenditure rays.}
#' \item{cons_grid_size}{The size for consumption grid lines.}
#' \item{cons_indiff_curve_size}{The size for consumption curve lines.}
#' \item{energy_grid_linetype}{The linetype for energy grid lines.}
#' \item{zero_perc_rebound_grid_linetype}{The linetype for 0% rebound grid lines.}
#' \item{hundred_perc_rebound_grid_linetype}{The linetype for the 100% rebound grid lines.}
#' \item{energy_rebound_lines_linetype}{The linetype for energy rebound lines.}
#' \item{expenditure_grid_linetype}{The linetype for expenditure grid lines.}
#' \item{expenditure_ray_linetype}{The linetype for expenditure rays.}
#' \item{cons_grid_linetype}{The linetype for consumption grid lines.}
#' \item{cons_indiff_curve_linetype}{The linetype for indifference curve lines.}
#' \item{n_indiff_curve_points}{The number of points on the indifference curves.}
#' \item{qs_qs0_lower}{The lower bound for the x value in the indifference curves.}
#' \item{qs_qs0_upper}{The upper bound for the x value in the indifference curves.}
#' \item{include_start_point}{A boolean that tells whether to include the starting point.}
#' \item{start_point_size}{The size of the start point.}
#' \item{start_point_shape}{The shape of the start point.}
#' \item{include_end_arrow}{A boolean that tells whether to include an ending arrow.}
#' \item{arrow_angle}{The angle for the arrow head, in degrees.}
#' \item{arrow_length}{The length of the arrow. See `grid::unit`.}
#' \item{arrow_type}{The arrow type, "closed" (the default) or "open". See `grid::arrow`.}
#' }
#' @examples
#' path_graph_params
"path_graph_params"


#' Sensitivity graph parameters
#' 
#' The list of graph parameters for drawing
#' sensitivity graphs in the `ReboundTools` package.
#' 
#' This list is passed to several graphing functions.
#' Callers may pass a modified version of this list 
#' to change sensitivity graph appearance.
#' 
#' @format A string list with `r length(sens_graph_params)` entries.
#' \describe{
#' 
#' \item{orig_point_shape}{The shape for points between rebound effects. Default is `1`, an open circle..} 
#' \item{orig_point_size}{The size for original points. Default is `2`.}
#' \item{orig_point_stroke}{The size of the line surrounding points between rebound effects. Default is `0.5`.}
#' \item{orig_point_colour}{The outline colour for original points. Default is "black".}
#' \item{orig_point_fill}{The fill colour for original points. Default is "black".}
#' \item{dempl_colour}{The colour for direct emplacment lines.}
#' \item{emb_colour}{The colour for embodied energy lines.}
#' \item{cap_colour}{The colour for capital expenditure lines.}
#' \item{md_colour}{The colour for maintenance and disposal lines.}
#' \item{empl_colour}{The colour for emplacement lines.}
#' \item{dsub_colour}{The colour for direct substitution lines.}
#' \item{isub_colour}{The colour for indirect substitution lines.}
#' \item{sub_colour}{The colour for substitution lines.}
#' \item{dinc_colour}{The colour for direct income lines.}
#' \item{iinc_colour}{The colour for indirect income lines.}
#' \item{inc_colour}{The colour for income lines.}
#' \item{macro_colour}{The colour for macro lines.}
#' \item{dir_colour}{The colour for direct lines.}
#' \item{indir_colour}{The colour for indirect lines.}
#' \item{tot_colour}{The colour for total rebound lines.}
#' \item{dempl_size}{The size for direct emplacment lines.}
#' \item{emb_size}{The size for embodied energy lines.}
#' \item{cap_size}{The size for capital expenditure lines.}
#' \item{md_size}{The size for maintenance and disposal lines.}
#' \item{empl_size}{The size for emplacement lines.}
#' \item{dsub_size}{The size for direct substitution lines.}
#' \item{isub_size}{The size for indirect substitution lines.}
#' \item{sub_size}{The size for substitution lines.}
#' \item{dinc_size}{The size for direct income lines.}
#' \item{iinc_size}{The size for indirect income lines.}
#' \item{inc_size}{The size for income lines.}
#' \item{macro_size}{The size for macro lines.}
#' \item{dir_size}{The size for direct lines.}
#' \item{indir_size}{The size for indirect lines.}
#' \item{tot_size}{The size for total rebound lines.}
#' \item{dempl_linetyps}{The linetype for direct emplacment lines.}
#' \item{emb_linetype}{The linetype for embodied energy lines.}
#' \item{cap_linetype}{The linetype for capital expenditure lines.}
#' \item{md_linetype}{The linetype for maintenance and disposal lines.}
#' \item{empl_linetype}{The linetype for emplacement lines.}
#' \item{dsub_linetype}{The linetype for direct substitution lines.}
#' \item{isub_linetype}{The linetype for indirect substitution lines.}
#' \item{sub_linetype}{The linetype for substitution lines.}
#' \item{dinc_linetype}{The linetype for direct income lines.}
#' \item{iinc_linetype}{The linetype for indirect income lines.}
#' \item{inc_linetype}{The linetype for income lines.}
#' \item{macro_linetype}{The linetype for macro lines.}
#' \item{dir_linetype}{The linetype for direct lines.}
#' \item{indir_linetype}{The linetype for indirect lines.}
#' \item{tot_linetype}{The linetype for total rebound lines.}
#' \item{lineend}{The line end style.}
#' \item{linejoin}{The line join style.}
#' \item{include_base_condition_points}{Tells whether to include base conditions points. Default is `TRUE`.}
#' \item{points_atop_paths}{Tells whether to draw points above paths (`TRUE`) or beneath paths (`FALSE`). Default is `TRUE`.}
#' \item{use_latex_legend}{Tells whether to convert legend text to LaTeX for a nicely-formatted legend. Default is `FALSE`.}
#' \item{include_x_axis}{Tells whether to add an x-axis at y = 0. Default is `FALSE`.}
#' }
#' @examples
#' sens_graph_params
"sens_graph_params"


#' Parametric analysis point types
#' 
#' This is the list of parametric analysis point types.
#' 
#' @format A string list with `r length(parametric_analysis_point_types)` entries.
#' \describe{
#' \item{point_type_colname}{The name of the column that contains parametric analysis point types.}
#' \item{orig}{The string for original points.}
#' \item{sweep}{The string for sweep points.}
#' }
#' @examples
#' parametric_analysis_point_types
"parametric_analysis_point_types"
