% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getConfInv.R
\name{meanBoot}
\alias{meanBoot}
\title{meanBoot function}
\usage{
meanBoot(data, indices)
}
\arguments{
\item{data}{is a vector of real-number values}

\item{indices}{is a vector of TRUE/FALSE indices. It allows boot to select samples.}
}
\value{
This function returns a mean of values in \code{data} that have values TRUE within \code{indices}.
}
\description{
meanBoot is a support function for bootstrapping method.
Its main purpose is to compute a mean of a given samples from  \code{data} selected by  \code{indices}.
}
