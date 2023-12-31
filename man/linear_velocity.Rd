% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MolecularDynamicsFunctions.R
\name{linear_velocity}
\alias{linear_velocity}
\title{Linear Velocity}
\usage{
linear_velocity(D_min, D_max, ROI, error)
}
\arguments{
\item{D_min}{Minimum step size.}

\item{D_max}{Maximum step size.}

\item{ROI}{A vector of two values representing the width and height of the region of interest, respectively.}

\item{error}{A vector of error values, as output by error function.}
}
\value{
Offsets for each point.
}
\description{
Calculates velocity, which scales linearly with error from minimum to maximum.
}
