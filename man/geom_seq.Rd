% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indifference.R
\name{geom_seq}
\alias{geom_seq}
\title{Generate a geometric sequence}
\usage{
geom_seq(from, to, n)
}
\arguments{
\item{from}{The first value in the sequence.}

\item{to}{The final value in the sequence.}

\item{n}{The number of points to include in the sequence.}
}
\value{
A vector of numbers.
}
\description{
Generate a geometric sequence
}
\examples{
geom_seq(from = 0.01, to = 10, n = 20)
geom_seq(from = 10, to = 20, n = 30)
}
