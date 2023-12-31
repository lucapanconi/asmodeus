% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MolecularDynamicsFunctions.R
\name{pointwise_ratiometric_error}
\alias{pointwise_ratiometric_error}
\title{Pointwise Ratiometric Error}
\usage{
pointwise_ratiometric_error(target, actual)
}
\arguments{
\item{target}{A list of targets, as output by target functions.}

\item{actual}{A list of actual point statistics, taken points input to target functions.}
}
\value{
A vector of error values, one for each point.
}
\description{
Calculates average ratiometric error of each point across a 2D curve. Compares each point to a distinct local target.
}
