% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paths.R
\name{energy_paths}
\alias{energy_paths}
\title{Title}
\usage{
energy_paths(
  .rebound_data,
  indexed = FALSE,
  I_E = ReboundTools::eeu_base_params$I_E,
  E_dot_s_orig = ReboundTools::orig_vars$E_dot_s_orig,
  E_dot_emb_orig = ReboundTools::orig_vars$E_dot_emb_orig,
  C_dot_md_orig = ReboundTools::orig_vars$C_dot_md_orig,
  C_dot_o_orig = ReboundTools::orig_vars$C_dot_o_orig,
  S_dot_dev = ReboundTools::star_vars$S_dot_dev,
  S_dot_dev_colour = ReboundTools::graph_colours$empl,
  S_dot_dev_size = 0.1,
  Delta_E_dot_emb_star = ReboundTools::Delta_vars$Delta_E_dot_emb_star,
  Delta_E_dot_emb_star_colour = ReboundTools::graph_colours$empl,
  Delta_E_dot_emb_star_size = 1,
  Delta_C_dot_md_star = ReboundTools::Delta_vars$Delta_C_dot_md_star,
  Delta_C_dot_md_star_I_E_colour = "black",
  Delta_C_dot_md_star_I_E_size = 0.5,
  Delta_E_dot_s_hat = ReboundTools::Delta_vars$Delta_E_dot_s_hat,
  Delta_E_dot_s_hat_colour = ReboundTools::graph_colours$sub,
  Delta_E_dot_s_hat_size = 1,
  Delta_C_dot_o_hat = ReboundTools::Delta_vars$Delta_C_dot_o_hat,
  Delta_C_dot_o_hat_I_E_colour = ReboundTools::graph_colours$sub,
  Delta_C_dot_o_hat_I_E_size = 1,
  Delta_E_dot_s_bar = ReboundTools::Delta_vars$Delta_E_dot_s_bar,
  Delta_E_dot_s_bar_colour = ReboundTools::graph_colours$inc,
  Delta_E_dot_s_bar_size = 0.5,
  Delta_C_dot_o_bar = ReboundTools::Delta_vars$Delta_C_dot_o_bar,
  Delta_C_dot_o_bar_I_E_colour = ReboundTools::graph_colours$inc,
  Delta_C_dot_o_bar_I_E_size = 0.5,
  k = ReboundTools::eeu_base_params$k,
  N_dot_hat = ReboundTools::hat_vars$N_dot_hat,
  prod_colour = ReboundTools::graph_colours$prod,
  prod_size = 1,
  graph_type = ReboundTools::graph_types$energy
)
}
\arguments{
\item{.rebound_data}{}
}
\value{

}
\description{
Title
}
\examples{
load_eeu_data() \%>\% 
  rebound_analysis() \%>\% 
  energy_paths()
}