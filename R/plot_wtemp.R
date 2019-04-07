#' Plot wtemp from netcdf file
#'
#' Extracts modelled water temperature from netcdf file and plots it using long.heatmap function
#'
#' @param file file; File path to netcdf file
#' @return filled point plot of water temperature
#' @export
plot_wtemp <- function(file, ...){
  wtemp <- get_var(ncdf = file, var = 'temp')
  z = get_var(ncdf = file, var = 'z')
  tmp = wide2long(wtemp,z)
  p1 = long_heatmap(tmp, ...)
  return(p1)
}
