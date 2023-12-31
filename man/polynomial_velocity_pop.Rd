% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MolecularDynamicsFunctions.R
\name{polynomial_velocity_pop}
\alias{polynomial_velocity_pop}
\title{Polynomial Velocity Multiple Populations}
\usage{
polynomial_velocity_pop(clouds, D_min, D_max, ROI, errors, n = 2)
}
\arguments{
\item{D_min}{Minimum step size.}

\item{D_max}{Maximum step size.}

\item{ROI}{A vector of two values representing the width and height of the region of interest, respectively.}

\item{n}{Factor of polynomial used in velocity calculation.}

\item{error}{A vector of error values, as output by error function.}
}
\value{
Offsets for each point.
}
\description{
Calculates velocity, which scales with error, by some polynomial of factor n, from minimum to maximum. Used for multiple populations.
}
