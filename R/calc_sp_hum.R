calc_sp_hum <- function(ta, airp, hum, method){

  a0 <- 6.107799961
  a1 <- 4.436518521E-1
  a2 <- 1.428945805E-2
  a3 <- 2.650648471E-4
  a4 <- 3.031240396E-6
  a5 <- 2.034080948E-8
  a6 <- 6.136820929E-11
  const06 <- 0.62198

  if(method == 'rhum'){
    rh = 0.01 * hum
    #saturation vapor pressure at that air temperature
    ea = a1 +ta*(a2+ta*(a3+ta*(a4+ta*(a5+ta*(a6+ta*a7)))))
    ea = ea * 100.0 # Conversion millibar --> Pascal
    #get actual vapor pressure
    ea = rh * ea
  }else if(method == 'dewt'){
    dew = hum
    ea = a1 +dew*(a2+dew*(a3+dew*(a4+dew*(a5+dew*(a6+dew*a7)))))
    ea = ea * 100.0 # Conversion millibar --> Pascal
  }else if(method == 'wet_bulb'){
    twet = hum
    ea = a1 +twet*(a2+twet*(a3+twet*(a4+twet*(a5+twet*(a6+twet*a7)))))
    ea = ea * 100.0 # Conversion millibar --> Pascal
    #actual vapor pressure
    ea = ea - 6.6e-4*(1+1.15e-3*twet)*airp*(ta-twet)
  }
  #convert to specific humidity kg/kg
  qa = const06*ea/(airp-0.377*ea)
  return(qa)
}
