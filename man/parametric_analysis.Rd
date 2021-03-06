% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parametric.R
\name{parametric_analysis}
\alias{parametric_analysis}
\title{Run parametric analyses}
\usage{
parametric_analysis(
  original_cases,
  parameterization,
  include_orig_point = TRUE,
  case_colname = ReboundTools::eeu_base_params$case,
  point_type_colname = ReboundTools::parametric_analysis_point_types$point_type_colname,
  orig = ReboundTools::parametric_analysis_point_types$orig,
  sweep = ReboundTools::parametric_analysis_point_types$sweep
)
}
\arguments{
\item{original_cases}{A data frame with baseline data from which parametric studies are desired.
This data frame is expected to contain 1 row, a single case.}

\item{parameterization}{A named list of lists. See details.}

\item{include_orig_point}{A boolean that tells whether the original point for each case (from \code{original_cases})
should be included in the output. Default is \code{TRUE}.}

\item{case_colname}{See \code{ReboundTools::eeu_base_params}.}

\item{point_type_colname, orig, sweep}{See \code{ReboundTools::parametric_analysis_point_types}.}
}
\value{
A data frame containing results of parametric studies.
}
\description{
After defining a base case for rebound analyses,
parametric analyses and sensitivity studies are
often helpful.
This function provides a convenient way to do parametric studies.
}
\details{
\code{original_cases} must have the same structure as the example data frames in this package.
See the file at \code{sample_eeu_data_path()}.
Failures will occur if \code{original_cases} contains columns calculated by \code{rebound_analysis()}.

\code{parameterization} is a list and must have the following structure.
Top-level elements are named for cases.
Case names must match the case column in \code{original_cases}.
Not every case in \code{original_cases} must be present.
Each top-level element in \code{parameterization} must itself also be a list.
Each 2nd-level list must contain named vectors, with each name corresponding
to a parameter in \code{original_cases} that will be swept
in the parametric study.
If more than one entry is included in a 2nd-level list,
all combinations of values will be used,
via \code{expand.grid()}.
Note that the original value of each parameter (in \code{original_cases})
will not be inserted into the \code{parameterization} lists,
so be sure to include that value, if desired.

Note that the case names in \code{parameterization} (i.e., the names of the top-level entries)
are filtering toward \code{original_cases}.
I.e., the output of this function will contain only those cases in \code{parameterization}.
}
\examples{
car_case <- load_eeu_data() \%>\% 
  dplyr::filter(.data[[ReboundTools::eeu_base_params$case]] == "Car")
# This example creates a parametric study for the "Car" case only,
# sweeping through values of both `k` and `p_E_engr_units`.
params <- list(Car = list(k = seq(0, 2, by = 1), 
                          p_E_engr_units = seq(1.5, 2.5, by = 0.25)))
res <- parametric_analysis(car_case, params)
dplyr::glimpse(res)
}
