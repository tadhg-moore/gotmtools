#' Extract fluxes from NetCDF file
#'
#' Extracts fluxes from the netCDF output file; Sensible heat flux (Qe), latent heat flux (Qh), long-wave back radation (Qb), net surface heat flux (heat).
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @return dataframe with the DateTime and heat fluxes in each columns.
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @export
get_flux <- function(ncdf){
  fid = nc_open(ncdf)
  tim = ncvar_get(fid, 'time')
  tunits = ncatt_get(fid,'time')
  #Extract time and formate Date
  lnam = tunits$long_name
  tustr <- strsplit(tunits$units, " ")
  step = tustr[[1]][1]
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  origin = as.POSIXct(paste0(tyear,'-',tmonth,'-',tday), format = '%Y-%m-%d', tz = 'UTC')
  time = as.POSIXct(tim, origin = origin, tz = 'UTC')

  qe = ncvar_get(fid, 'qe')
  qh = ncvar_get(fid, 'qh')
  qb = ncvar_get(fid, 'qb')
  heat = ncvar_get(fid, 'heat')
  nc_close(fid)

  #Extract time and formate Date
  df <- data.frame(DateTime = time, Qe = qe, Qh = qh, Qb = qb, heat = heat)
  return(df)
}
