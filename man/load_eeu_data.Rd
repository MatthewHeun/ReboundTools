% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example_data.R
\name{load_eeu_data}
\alias{load_eeu_data}
\title{Load EED data from a sheet in an Excel workbook}
\usage{
load_eeu_data(
  path = sample_eeu_data_path(),
  sheet = ReboundTools::eeu_data_table$eeu_data_sheet,
  expected_cols = ReboundTools::eeu_base_data
)
}
\arguments{
\item{path}{}

\item{sheet}{}
}
\value{

}
\description{
Energy efficiency upgrade (EEU) data are often stored in an Excel workbook.
This function reads those data.
}