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
#' \item{bar}{After the income effect and before the productivity effect.}
#' \item{tilde}{After the productivity effect.}
#' \item{I}{An economic intensity (per $).}
#' \item{p}{Price.}
#' \item{E}{Energy, typically final energy.}
#' \item{s}{Energy service.}
#' \item{o}{Other goods.}
#' \item{eta}{Energy service efficiency, calculated by energy service divided by final energy consumed to provide that service.}
#' \item{eps}{Elasticity.}
#' \item{UC}{Uncompensated.}
#' \item{dot}{Signifies a rate, typically per year.}
#' \item{q}{Quantity of energy service or other goods consumed.}
#' \item{M}{Income.}
#' \item{C}{Cost.}
#' \item{t}{Expected device lifetime.}
#' }
#' 
#' @format A string list with `r length(eeu_base_params)` entries.
#' \describe{
#' \item{case}{A string to identify the case being analyzed, e.g., "Lamp".}
#' \item{original}{A string to identify the original device, e.g., "Incandescent".}
#' \item{upgrade}{A string to identify the upgraded device, e.g., "LED".}
#' \item{MJ_engr_unit}{A unit conversion factor: the number of MJ per engineering unit for the service efficiency. For example, if the service efficiency is given in miles/gallon, `MJ_engr_unit` should be 126.6 MJ/gallon. This unit conversion number is used in calculating the actual service efficiency.}
#' \item{I_E}{The energy intensity of the economy \[MJ/$\].}
#' \item{k}{The productivity effect factor \[--\].}
#' \item{p_E}{The price of energy \[$/MJ\].}
#' \item{eta_orig_engr_units}{The original (pre-EEU) energy service efficiency.  This number should have engineering units in the denominator, e.g., \[miles/gallon\] \[lumens/kW\]. Note that the denominator unit of `eta_orig_engr_units` is assumed to be the same as the denominator unit of `MJ_engr_unit`.}
#' \item{eta_star_engr_units}{The upgraded (post-EEU) energy service efficiency. This number should have engineering units in the denominator, e.g., \[miles/gallon\] \[lumens/kW\]. Note that the denominator unit of `eta_orig_engr_units` is assumed to be the same as the denominator unit of `MJ_engr_unit`.}
#' \item{e_qs_ps_UC}{The uncompensated ("UC") energy service price ("ps") elasticity ("e") of energy service ("qs") consumption (own-price elasticity) \[--\].}
#' \item{e_qs_M}{The income ("M") elasticity ("e") of energy service ("qs") consumption \[--\].}
#' \item{e_qo_M}{The income ("M") elasticity ("e") of other goods ("qo") consumption \[--\].}
#' \item{q_dot_s_orig}{The original (pre-EEU) consumption rate of the energy service. Example units are \[miles/year\] \[lumen-hours/year\].}
#' \item{M_dot_orig}{The disposable income rate, exclusive of taxes and savings \[$/year\].}
#' \item{C_cap_orig}{The net capital cost of the original device: the sum of purchase price and financing costs less rebates and resale value at end of ownership \[$\].}
#' \item{t_orig}{The expected lifetime of the original device \[year\].}
#' \item{C_cap_star}{The net capital cost of the upgraded device: the sum of purchase price and financing costs less rebates and resale value at end of ownership \[$\].}
#' \item{t_star}{The expected lifetime of the upgraded device \[year\].}
#' \item{C_dot_md_orig}{The original (pre-EEU) maintenance and disposal cost rate \[$/year\].}
#' \item{C_dot_md_star}{The upgraded (post-EEU) maintenance and disposal cost rate \[$/year\].}
#' \item{E_emb_orig}{The embodied energy of the original (pre-EEU) device \[MJ\].}
#' \item{E_emb_star}{The embodied energy of the upgraded (post-EEU) device \[MJ\].}
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
#' \item{eta_orig}{Energy service efficiency of the original (pre-EEU) device on a per-MJ basis \[service/MJ\], calculated by `eta_orig_engr_units / MJ_engr_unit`.}
#' \item{E_dot_s_orig}{The final energy consumption rate of the original (pre-EEU) device \[MJ/year\], calculated by `q_dot_s_orig / eta_orig`.}
#' \item{C_dot_cap_orig}{The original (pre-EEU) capital cost rate \[$/year\], calculated by `C_cap_orig / t_orig`.}
#' \item{p_s_orig}{The original (pre-EEU) energy service price \[$/service\], calculated by `p_E / eta_orig`.}
#' \item{C_dot_s_orig}{The original (pre-EEU) rate of energy cost expenditures for the device \[$/year\], calculated by `p_E * E_dot_s_orig`.}
#' \item{C_dot_o_orig}{The original (pre-EEU) rate of expenditure on other goods \[$/year\], calculated by `M_dot_orig - C_dot_s_orig - C_dot_cap_orig - C_dot_md_orig`.}
#' \item{f_Cs_orig}{The original (pre-EEU) fraction of the energy and other budget spent on the energy service \[--\], calculated by `C_dot_s_orig / (C_dot_s_orig + C_dot_o_orig)`.}
#' \item{e_qs_ps}{The energy service price ("ps") elasticity ("e") of energy service ("qs") consumption \[--\], calculated by `e_qs_ps_UC_val + f_Cs_orig_val*e_qs_M_val`.}
#' \item{e_qo_ps}{The energy service price ("ps") elasticity ("e") of other goods ("qo") consumption \[--\], calculated by `f_Cs_orig_val*(f_Cs_orig_val + e_qs_ps_UC_val) / (f_Cs_orig_val - 1)`.}
#' \item{E_dot_emb_orig}{The original (pre-EEU) rate of embodied energy demand\[MJ/year\], calculated by `E_dot_emb_val / t_orig_val`.}
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
#' \item{eta_star}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], calculated by `eta_tilde_engr_units / MJ_engr_unit`.}
#' \item{eta_ratio}{The energy service efficiency ratio \[--\], calculated by `eta_star/eta_orig`.}
#' 
#' 
#' 
#' 
#' \item{E_dot_s_star}{The final energy consumption rate of the original (pre-EEU) device \[MJ/year\], calculated by `q_dot_s_orig / eta_orig`.}
#' \item{S_dot_dev}{The expected device-level energy savings rate \[MJ/year\], calculated by `(eta_ratio - 1) * (1/eta_ratio) * E_dot_s_orig`.}
#' \item{G_dot}{The expected device-level energy gross cost savings rate \[MJ/year\], calculated by `p_E * S_dot_dev`.}
#' \item{C_dot_cap_star}{The upgraded (post-EEU) capital cost rate \[$/year\], calculated by `C_cap_star / t_star`.}
#' \item{p_s_star}{The upgraded (post-EEU) energy service price \[$/service\], calculated by `p_E / eta_star = p_E / eta_tilde`.}
#' \item{q_dot_s_star}{The upgraded (post-EEU) energy service consumption rate \[service/year\], same as `q_dot_s_orig`.}
#' }
#' @examples
#' star_vars
# "star_vars"
