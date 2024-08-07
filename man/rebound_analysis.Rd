% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_rebound_data.R
\name{rebound_analysis}
\alias{rebound_analysis}
\title{Perform a complete rebound analysis}
\usage{
rebound_analysis(.eeu_data)
}
\arguments{
\item{.eeu_data}{Energy efficiency upgrade information in a data frame.
See \code{load_eeu_data()} for an example data frame.}
}
\value{
\code{.eeu_data} with all rebound terms added as columns to the right.
}
\description{
This function calls all rebound analysis functions in the correct order
}
\examples{
complicated <- load_eeu_data() \%>\% 
  calc_orig() \%>\% 
  calc_star() \%>\% 
  calc_hat() \%>\% 
  calc_bar() \%>\% 
  calc_tilde() \%>\% 
  calc_Deltas() \%>\% 
  calc_rebound()
simple <- load_eeu_data() \%>\% 
  rebound_analysis()
all.equal(complicated, simple)
}
