#' Plot fluxes from NetCDF file
#'
#' Plots fluxes from the netCDF output file; Sensible heat flux (Qe), latent heat flux (Qh), long-wave back radation (Qb), net surface heat flux (heat).
#'
#' @param ncdf filepath; Name of the netCDF file to extract variable
#' @param title character; Title of the graph. Defaults to 'Heat Fluxes'.
#' @return dataframe with the
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 nc_close
#' @importFrom ncdf4 ncvar_get
#' @importFrom ncdf4 ncatt_get
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
plot_flux <- function(ncdf, title = 'Heat Fluxes'){
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

  #I0 = ncvar_get(fid, 'I_0') # Short wave radiation
  qe = ncvar_get(fid, 'qe')
  qh = ncvar_get(fid, 'qh')
  qb = ncvar_get(fid, 'qb')
  heat = ncvar_get(fid, 'heat')
  nc_close(fid)

  #Extract time and formate Date
  df <- data.frame(DateTime = time, Qe = as.vector(qe), Qh = as.vector(qh), Qb = as.vector(qb), heat = as.vector(heat))
  dfmlt <- reshape2::melt(df, id.vars = 'DateTime')
  colnames(dfmlt) <- c('DateTime', 'Flux', 'value')
  #Plot data
  p1 <- ggplot(dfmlt, aes(DateTime, value, colour = Flux))+
    geom_line(size = 0.8)+
    ggtitle(title)+
    xlab('')+
    geom_hline(yintercept = 0, colour = 'black')+
    ylab('W/m^2')+
    theme_bw(base_size = 18)
  p1

  return(p1)
}
