% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MolecularDynamicsFunctions.R
\name{ripley_target_pop}
\alias{ripley_target_pop}
\title{Main Euclidean Local Ripley's Target for Multiple Populations}
\usage{
ripley_target_pop(points, ROI, R, lambda, distances)
}
\arguments{
\item{points}{A matrix or data frame with two columns: x and y coordinates of each point.}

\item{ROI}{A vector of two values representing the width and height of the region of interest, respectively.}

\item{R}{A vector of radial values for which to calculate the K function over.}

\item{lambda}{Numeric value, used to weight the K function. Calculated automatically, should not be altered.}

\item{distances}{A distance matrix between all points.}
}
\value{
A list of the local K functions, one for each row (point) in points.
}
\description{
Calculates the localised, linearised Ripley's K function for each point of a given population compared to all other populations.
}
