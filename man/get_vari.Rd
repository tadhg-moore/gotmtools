% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_vari.R
\name{get_vari}
\alias{get_vari}
\title{Extract variables from NetCDF file}
\usage{
get_vari(ncdf, var, print = T)
}
\arguments{
\item{ncdf}{filepath; Name of the netCDF file to extract variable}

\item{var}{character; Name of the variable to be extracted. Must match short name in netCDF file}

\item{print}{logical; Print the name and units of the variable extracted, defaults to TRUE}
}
\value{
dataframe in the same format as the observation file with the surface in the top column and the bottom in the last column.
}
\description{
Extracts a selected parameter from the netCDF file and formats it into a dataframe.
}
