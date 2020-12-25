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




#' EEU base data 
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
#' @format A string list with `r length(eeu_base_data)` entries.
#' \describe{
#' \item{case}{A string to identify the case being analyzed, e.g., "Lamp".}
#' \item{original}{A string to identify the original device, e.g., "Incandescent".}
#' \item{upgrade}{A string to identify the upgraded device, e.g., "LED".}
#' \item{I_E}{The energy intensity of the economy \[MJ/$\].}
#' \item{k}{The productivity effect factor \[--\].}
#' \item{p_E}{The price of energy \[$/MJ\].}
#' \item{eta_orig}{The original (pre-EEU) energy service efficiency \[miles/gallon\] \[lumens/W\]. Note that the service can be in any units, such as lumens/W or miles/gallon. Only the energy service efficiency ratio (`eta_tilde/eta_orig`) matters.}
#' \item{eta_tilde}{The upgraded (post-EEU) energy service efficiency \[miles/gallon\] \[lumens/W\]. Note that the service can be in any units, such as lumens/W or miles/gallon. Only the energy service efficiency ratio (`eta_tilde/eta_orig`) matters.}
#' \item{eps_ps_qs_UC}{The uncompensated energy service price elasticity of energy service consumption (own-price elasticity) \[--\].}
#' \item{eps_qs_M}{The income elasticity of energy service consumption \[--\].}
#' \item{eps_qo_M}{The income elasticity of other goods consumption \[--\].}
#' \item{q_dot_s_orig}{The original (pre-EEU) consumption rate of the energy service \[miles/year\] \[lumen-hours/year\].}
#' \item{M_dot_orig}{The disposable income rate, exclusive of taxes and savings \[$/year\].}
#' \item{C_cap_orig}{The net capital cost of the original device: the sum of purchase price and financing costs less rebates and resale value at end of ownership \[$\].}
#' \item{t_orig}{The expected lifetime of the original device \[year\].}
#' \item{C_cap_star}{The net capital cost of the upgraded device: the sum of purchase price and financing costs less rebates and resale value at end of ownership \[$\].}
#' \item{t_star}{The expected lifetime of the upgraded device \[year\].}
#' \item{C_dot_md_orid}{The original (pre-EEU) maintenance and disposal cost rate \[$/year\].}
#' \item{C_dot_md_star}{The upgraded (post-EEU) maintenance and disposal cost rate \[$/year\].}
#' }
#' 
#' @examples
#' eeu_base_data
"eeu_base_data"



#' EEU derived data 
#' 
#' This is the list of the derived parameters for a rebound analysis. 
#' Each of these parameters is calculated data in `ReboundTools::eeu_base_data`.
#' 
#' @format A string list with `r length(eeu_base_data)` entries.
#' \describe{
#' \item{eta_ratio}{The ratio `eta_tilde/eta_orig`.}
#' }
#' @examples
#' eeu_derived_data
"eeu_derived_data"
