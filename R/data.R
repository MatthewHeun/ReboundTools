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
#' \item{time_unit}{The time unit ("year").}
#' \item{currency_unit}{The default currency unit ("$").}
#' \item{currency_unit_latex}{The currency unit in LaTeX format ("\\$").}
#' \item{unitless}{The identifier for unitless variables ("-").}
#' \item{unitless_latex}{The identifier for unitless variables in LaTeX format ("--").}
#' \item{leading_delta_pattern}{A regex pattern that identifies a leading Delta ("^Delta_").}
#' \item{surround_left}{The left delimiter for units ("\["]).}
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
#' \item{eta}{Energy service efficiency, calculated by energy service divided by final energy consumed to provide that service.}
#' \item{p_s}{Energy service price \[service/MJ\], calculated by `p_E/eta`.}
#' \item{q_dot_s}{The rate of energy service consumption \[service/year\], calculated by `eta*E_dot_s`.}
#' \item{E_dot_s}{The rate of final energy consumption by the energy conversion device \[MJ/year\].}
#' \item{E_dot_emb}{The rate of embodied energy demand by the energy conversion device \[MJ/year\], calculated by `E_emb/t_life`.}
#' \item{C_dot_s}{The cost rate of energy consumption by the device \[$/year\], calculated by `p_s*q_dot_s`.}
#' \item{C_dot_cap}{The capital cost rate of the device \[$/year\], calculated by `C_cap/t_own`.}
#' \item{C_dot_md}{The maintenance and disposal cost rate of the device \[$/year\].}
#' \item{C_dot_o}{The other goods consumption rate \[$/year\], calculated, initially, as a residual of the budget constraint.}
#' \item{M_dot}{Real income \[$/year\].}
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
#' \item{bar}{After the income effect but before the productivity effect.}
#' \item{tilde}{After the productivity effect.}
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
#' \item{bar}{After the income effect and before the productivity effect.}
#' \item{tilde}{After the productivity effect.}
#' \item{I}{An economic intensity (per $).}
#' \item{p}{Price.}
#' \item{E}{Energy, typically final energy.}
#' \item{s}{Energy service.}
#' \item{o}{Other goods.}
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
#' \item{reference}{A string for a reference for this case, e.g. bibliographic entry for a paper in which this case appears.}
#' \item{case}{A string to identify the case being analyzed, e.g., "Lamp".}
#' \item{original}{A string to identify the original device, e.g., "Incandescent".}
#' \item{upgrade}{A string to identify the upgraded device, e.g., "LED".}
#' \item{service_unit}{A string to identify the unit of the energy service, e.g., "miles" in "miles/gal" or "lm-hr" in "lm-hr/kW-hr".}
#' \item{energy_engr_unit}{A string to identify the energy units of the service, e.g., "gal" in "miles/gal" or "kW-hr" in "lm-hr/kW-hr".}
#' \item{MJ_engr_unit}{A unit conversion factor: the number of MJ per engineering unit for the service efficiency. For example, if the service efficiency is given in miles/gallon, `MJ_engr_unit` should be 126.6 MJ/gallon. This unit conversion number is used in calculating the actual service efficiency.}
#' \item{I_E}{The energy intensity of the economy \[MJ/$\].}
#' \item{k}{The productivity effect factor \[--\].}
#' \item{p_E_engr_units}{The price of energy in engineering units, e.g., $/gal or $/kW-hr \[$/energy_engr_unit\].}
#' \item{e_qs_ps_UC}{The uncompensated ("UC") energy service price ("ps") elasticity ("e") of energy service ("qs") consumption (own-price elasticity) \[--\].}
#' \item{e_qs_M}{The income ("M") elasticity ("e") of energy service ("qs") consumption \[--\].}
#' \item{e_qo_M}{The income ("M") elasticity ("e") of other goods ("qo") consumption \[--\].}
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
#' \item{p_E}{The price of energy \[$/MJ\], calculated by `p_E_engr_units / MJ_engr_unit`.}
#' \item{q_dot_s_orig}{The original (pre-EEU) consumption rate of the energy service. Example units are \[miles/year\] \[lumen-hours/year\].}
#' \item{C_cap_orig}{The net capital cost of the original device: the sum of purchase price and financing costs less rebates and resale value at end of ownership \[$\].}
#' \item{M_dot_orig}{The disposable income rate, exclusive of taxes and savings \[$/year\].}
#' \item{t_own_orig}{The expected ownership duration of the original device \[year\].}
#' \item{C_dot_md_orig}{The original (pre-EEU) maintenance and disposal cost rate \[$/year\].}
#' \item{eta_engr_units_orig}{The original (pre-EEU) energy service efficiency.  This number should have engineering units in the denominator, e.g., \[miles/gallon\] \[lumens/kW\]. Note that the denominator unit of `eta_engr_units_orig` is assumed to be the same as the denominator unit of `MJ_engr_unit`.}
#' \item{eta_orig}{Energy service efficiency of the original (pre-EEU) device on a per-MJ basis \[service/MJ\], calculated by `eta_engr_units_orig / MJ_engr_unit`.}
#' \item{E_dot_s_orig}{The final energy consumption rate of the original (pre-EEU) device \[MJ/year\], calculated by `q_dot_s_orig / eta_orig`.}
#' \item{C_dot_cap_orig}{The original (pre-EEU) capital cost rate \[$/year\], calculated by `C_cap_orig / t_orig`.}
#' \item{p_s_orig}{The original (pre-EEU) energy service price \[$/service\], calculated by `p_E / eta_orig`.}
#' \item{C_dot_s_orig}{The original (pre-EEU) rate of energy cost expenditures for the device \[$/year\], calculated by `p_E * E_dot_s_orig`.}
#' \item{C_dot_o_orig}{The original (pre-EEU) rate of expenditure on other goods \[$/year\], calculated by `M_dot_orig - C_dot_s_orig - C_dot_cap_orig - C_dot_md_orig`.}
#' \item{f_Cs_orig}{The original (pre-EEU) fraction of the energy and other budget spent on the energy service \[--\], calculated by `C_dot_s_orig / (C_dot_s_orig + C_dot_o_orig)`.}
#' \item{e_qs_ps}{The energy service price ("ps") elasticity ("e") of energy service ("qs") consumption \[--\], calculated by `e_qs_ps_UC + f_Cs_orig*e_qs_M`.}
#' \item{e_qo_ps}{The energy service price ("ps") elasticity ("e") of other goods ("qo") consumption \[--\], calculated by `f_Cs_orig*(f_Cs_orig + e_qs_ps_UC) / (f_Cs_orig - 1)`.}
#' \item{sigma}{The elasticity of substitution between energy service consumption and other goods consumption \[--\].}
#' \item{E_emb_orig}{The embodied energy of the original (pre-EEU) device \[MJ\].}
#' \item{t_life_orig}{The expected lifetime of the original (pre-EEU) device \[year\].}
#' \item{E_dot_emb_orig}{The original (pre-EEU) rate of embodied energy demand\[MJ/year\], calculated by `E_dot_emb / t_orig`.}
#' \item{N_dot_orig}{The original (pre-EEU) freed cash rate \[$/year\], exactly `0`.}
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
#' \item{C_cap_star}{The net capital cost of the upgraded device: the sum of purchase price and financing costs less rebates and resale value at end of ownership \[$\].}
#' \item{t_own_star}{The expected ownership duration of the upgraded device \[year\].}
#' \item{C_dot_md_star}{The upgraded (post-EEU) maintenance and disposal cost rate \[$/year\].}
#' \item{E_emb_star}{The embodied energy of the upgraded (post-EEU) device \[MJ\].}
#' \item{t_life_star}{The expected lifetime of the upgraded (post-EEU) device \[year\].}
#' \item{eta_star_engr_units}{The upgraded (post-EEU) energy service efficiency. This number should have engineering units in the denominator, e.g., \[miles/gallon\] \[lumens/kW\]. Note that the denominator unit of `eta_engr_units_orig` is assumed to be the same as the denominator unit of `MJ_engr_unit`.}
#' \item{eta_star}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], calculated by `eta_tilde_engr_units / MJ_engr_unit`.}
#' \item{eta_ratio}{The energy service efficiency ratio \[--\], calculated by `eta_star/eta_orig`.}
#' \item{S_dot_dev}{The expected device-level energy savings rate \[MJ/year\], calculated by `(eta_ratio - 1) * (1/eta_ratio) * E_dot_s_orig`.}
#' \item{G_dot}{The expected device-level energy gross cost savings rate \[MJ/year\], calculated by `p_E * S_dot_dev`.}
#' \item{p_s_star}{The upgraded (post-EEU) energy service price \[$/service\], calculated by `p_E / eta_star = p_E / eta_tilde`.}
#' \item{q_dot_s_star}{The upgraded (post-EEU) energy service consumption rate \[service/year\], same as `q_dot_s_orig`.}
#' \item{C_dot_cap_star}{The upgraded (post-EEU) capital cost rate \[$/year\], calculated by `C_cap_star / t_star`.}
#' \item{E_dot_emb_star}{The upgraded (post-EEU) embodied energy rate \[MJ/year\], calculated by `E_emb_star / t_star`.}
#' \item{C_dot_s_star}{The upgraded (post-EEU) energy cost rate \[$/year\], calculated by `p_s_star * q_dot_s_star`.}
#' \item{M_dot_star}{The disposable income rate, exclusive of taxes and savings \[$/year\], exactly `M_dot_orig`.}
#' \item{N_dot_star}{The freed cash rate \[$/year\], calculated by `G_dot - (C_dot_cap_star - C_dot_cap_orig) - (C_dot_md_star - C_dot_md_orig)`.}
#' \item{C_dot_o_star}{The upgraded (post-EEU) other goods expenditure rate \[$/year\], exactly `C_dot_o_orig`.}
#' \item{E_dot_s_star}{The upgraded (post-EEU) energy consumption rate \[MJ/year\], calculated by `q_dot_s_star / eta_star`.}
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
#' \item{eta_hat}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], exactly `eta_star`.}
#' \item{p_s_hat}{The energy service price after the substitution effect \[$/service\], exactly `p_s_star`.}
#' \item{C_dot_cap_hat}{The capital expenditure rate after the substitution effect \[$/year\], exactly `C_dot_cap_star`.}
#' \item{C_dot_md_hat}{The maintenance and disposal expenditure rate after the substitution effect \[$/year\], exactly `C_dot_md_star`.}
#' \item{E_dot_emb_hat}{The embodied energy rate after the substitution effect \[MJ/year\], exactly `E_dot_emb_star`.}
#' \item{M_dot_hat}{Real income after the substitution effect \[MJ/year\], exactly `M_dot_star`.}
#' \item{q_dot_s_hat}{The rate of energy service consumption after the substitution effect\ [service/year\], calculated by `q_dot_s_star * eta_ratio^(-e_qs_ps)`.}
#' \item{E_dot_s_hat}{The rate of energy consumption after the substitution effect\ [service/year\], calculated by `q_dot_s_hat / eta_hat`.}
#' \item{C_dot_o_hat}{The rate of other goods expenditures after the substitution effect \[$/year\], calculated by `C_dot_o_star * eta_ratio^(-e_qo_ps)`.}
#' \item{N_dot_hat}{The freed cash rate \[$/year\], calculated by `G_dot - (C_dot_cap_star - C_dot_cap_orig) - (C_dot_md_star - C_dot_md_orig)`.}
#' \item{M_dot_hat_prime}{Modified `M_dot` for the income effect \[$/year\], calculated by `M_dot_hat - C_dot_cap_orig - C_dot_md_orig - G_dot + p_E*(E_dot_s_hat - E_dot_s_star) + (C_dot_o_hat - C_dot_o_star)`}
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
#' \item{eta_bar}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], exactly `eta_hat`.}
#' \item{p_s_bar}{The energy service price after the income effect \[$/service\], exactly `p_s_hat`.}
#' \item{C_dot_cap_bar}{The capital expenditure rate after the income effect \[$/year\], exactly `C_dot_cap_hat`.}
#' \item{C_dot_md_bar}{The maintenance and disposal expenditure rate after the income effect \[$/year\], exactly `C_dot_md_hat`.}
#' \item{E_dot_emb_bar}{The embodied energy rate after the income effect \[MJ/year\], exactly `E_dot_emb_hat`.}
#' \item{M_dot_bar}{Real income after the income effect \[MJ/year\], exactly `M_dot_hat`.}
#' \item{q_dot_s_bar}{The rate of energy service consumption after the income effect\ [service/year\], calculated by `(1 + N_dot_hat/M_dot_hat_prime)^(e_qs_M)`.}
#' \item{C_dot_o_bar}{The rate of other goods expenditures after the income effect \[$/year\], calculated by `(1 + N_dot_hat/M_dot_hat_prime)^(e_qo_M)`.}
#' \item{N_dot_bar}{The freed cash rate after the income effect \[$/year\], exactly `0`.}
#' }
#' @examples
#' bar_vars
"bar_vars"


#' EEU tilde data 
#' 
#' This is the list of the derived variables at the tilde stage (after productivity effect) of a rebound analysis.
#' 
#' @format A string list with `r length(tilde_vars)` entries.
#' \describe{
#' \item{eta_tilde}{Energy service efficiency of the upgraded (post-EEU) device on a per-MJ basks \[service/MJ\], exactly `eta_bar`.}
#' \item{p_s_tilde}{The energy service price after the productivity effect \[$/service\], exactly `p_s_bar`.}
#' \item{C_dot_cap_tilde}{The capital expenditure rate after the productivity effect \[$/year\], exactly `C_dot_cap_bar`.}
#' \item{C_dot_md_tilde}{The maintenance and disposal expenditure rate after the productivity effect \[$/year\], exactly `C_dot_md_bar`.}
#' \item{E_dot_emb_tilde}{The embodied energy rate after the productivity effect \[MJ/year\], exactly `E_dot_emb_bar`.}
#' \item{M_dot_tilde}{Real income after the productivity effect \[MJ/year\], exactly `M_dot_bar`.}
#' \item{q_dot_s_tilde}{The rate of energy service consumption after the productivity effect\ [service/year\], exactly `q_dot_s_bar`.}
#' \item{C_dot_o_tilde}{The rate of other goods expenditures after the productivity effect \[$/year\], exactly `C_dot_o_bar`.}
#' \item{N_dot_tilde}{The freed cash rate after the productivity effect \[$/year\], exactly `0`.}
#' }
#' @examples
#' tilde_vars
"tilde_vars"


#' Rebound terms
#' 
#' This is the list of the rebound terms 
#' 
#' @format A string list with `r length(rebound_terms)` entries.
#' \describe{
#' \item{Re_dempl}{Direct emplacement effect rebound, always 0.}
#' \item{Re_emb}{Indirect embodied energy effect rebound.}
#' \item{Re_md}{Indirect maintenance and disposal effect rebound.}
#' \item{Re_empl}{Emplacement effect rebound.}
#' \item{Re_dsub}{Direct substitution effect rebound.}
#' \item{Re_isub}{Indirect substitution effect rebound.}
#' \item{Re_sub}{Substitution effect rebound.}
#' \item{Re_dinc}{Direct income effect rebound.}
#' \item{Re_iinc}{Indirect income effect rebound.}
#' \item{Re_inc}{Income effect rebound.}
#' \item{Re_prod}{Indirect productivity effect rebound.}
#' \item{Re_d}{Sum of all direct rebound effects.}
#' \item{Re_i}{Sum of all indirect rebound effects.}
#' \item{Re_tot}{Total rebound.}
#' }
#' @examples
#' rebound_terms
"rebound_terms"


#' Graph types
#' 
#' The list of graph types in the `ReboundTools` package.
#' 
#' @format A string list with `r length(graph_types)` entries.
#' \describe{
#' \item{energy}{The energy graph type.}
#' \item{cost}{The cost graph type.}
#' \item{preferences}{The preferences graph type.}
#' }
#' @examples
#' graph_types
"graph_types"


#' Graph parameters
#' 
#' The list of graph parameters for drawing
#' energy path graphs, cost path graphs, and preference path graphs
#' in the `ReboundTools` package.
#' 
#' This list is passed to several graphing functions.
#' Callers may pass a modified version of this list 
#' to change graph appearance.
#' 
#' @format A string list with `r length(default_graph_params)` entries.
#' \describe{
#' 
#' \item{lineend}{The line end style.}
#' \item{linejoin}{The line join style.}
#' \item{linejoin}{The line join style.}
#' \item{dempl_colour}{The colour for direct emplacment lines.}
#' \item{emb_colour}{The colour for embodied energy lines.}
#' \item{cap_colour}{The colour for capital cost lines.}
#' \item{md_colour}{The colour for maintenance and disposal lines.}
#' \item{dsub_colour}{The colour for direct substitution lines.}
#' \item{isub_colour}{The colour for indirect substitution lines.}
#' \item{dinc_colour}{The colour for direct income lines.}
#' \item{iinc_colour}{The colour for indirect income lines.}
#' \item{prod_colour}{The colour for productivity lines.}
#' \item{dempl_size}{The size for direct emplacment lines.}
#' \item{emb_size}{The size for embodied energy lines.}
#' \item{cap_size}{The size for capital cost lines.}
#' \item{md_size}{The size for maintenance and disposal lines.}
#' \item{dsub_size}{The size for direct substitution lines.}
#' \item{isub_size}{The size for indirect substitution lines.}
#' \item{dinc_size}{The size for direct income lines.}
#' \item{iinc_size}{The size for indirect income lines.}
#' \item{prod_size}{The size for productivity lines.}
#' \item{dempl_linetyps}{The linetype for direct emplacment lines.}
#' \item{emb_linetype}{The linetype for embodied energy lines.}
#' \item{cap_linetype}{The linetype for capital cost lines.}
#' \item{md_linetype}{The linetype for maintenance and disposal lines.}
#' \item{dsub_linetype}{The linetype for direct substitution lines.}
#' \item{isub_linetype}{The linetype for indirect substitution lines.}
#' \item{dinc_linetype}{The linetype for direct income lines.}
#' \item{iinc_linetype}{The linetype for indirect income lines.}
#' \item{prod_linetype}{The linetype for productivity lines.}
#' \item{energy_grid_colour}{The colour for energy grid lines.}
#' \item{energy_rebound_lines_colour}{The colour for energy rebound lines.}
#' \item{cost_grid_colour}{The colour for cost grid lines.}
#' \item{cost_ray_colour}{The colour for cost rays.}
#' \item{prefs_grid_colour}{The colour for preferences grid lines.}
#' \item{prefs_indiff_curve_colour}{The colour for indifference curve lines.}
#' \item{energy_grid_size}{The size for energy grid lines.}
#' \item{energy_rebound_lines_size}{The size for energy rebound lines.}
#' \item{cost_grid_size}{The size for cost grid lines.}
#' \item{cost_ray_size}{The size for cost rays.}
#' \item{prefs_grid_size}{The size for preferences grid lines.}
#' \item{prefs_indiff_curve_size}{The size for indifference curve lines.}
#' \item{energy_grid_linetype}{The linetype for energy grid lines.}
#' \item{energy_rebound_lines_linetype}{The linetype for energy rebound lines.}
#' \item{cost_grid_linetype}{The linetype for cost grid lines.}
#' \item{cost_ray_linetype}{The linetype for cost rays.}
#' \item{prefs_grid_linetype}{The linetype for preferences grid lines.}
#' \item{prefs_indiff_curve_linetype}{The linetype for indifference curve lines.}
#' \item{include_start_point}{A boolean that tells whether to include the starting point.}
#' \item{start_point_size}{The size of the start point.}
#' \item{start_point_shape}{The shape of the start point.}
#' \item{include_end_arrow}{A boolean that tells whether to include an ending arrow.}
#' \item{arrow_angle}{The angle for the arrow head, in degrees.}
#' \item{arrow_length}{The length of the arrow. See `grid::unit`.}
#' \item{arrow_type}{The arrow type, "closed" (the default) or "open". See `grid::arrow`.}
#' }
#' @examples
#' default_graph_params
"default_graph_params"


#' Graph data frame columns
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
#' \item{start_point_col}{The name of the boolean column telling whether this row contains a segment that should have a starting point.}
#' \item{end_arrow_col}{The name of the boolean column telling whether this row contains a segment that should have an ending arrow.}
#' }
#' @examples
#' graph_df_colnames
"graph_df_colnames"
