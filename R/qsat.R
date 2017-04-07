#' @title Saturation Specific Humidity
#'
#' @param Ta air temperature [deg C]
#' @param Pa pressure [mb]
#'
#' @description
#'  Returns saturation specific humidity [kg/kg] given
#'  the air temperature and pressure. Useful in convering
#'  specific humitity to relative humidity
#'
qsat = function(Ta, Pa){
  ew = 6.1121*(1.0007+3.46e-6*Pa)*exp((17.502*Ta)/(240.97+Ta)) # in mb
  q  = 0.62197*(ew/(Pa-0.378*ew))                              # mb -> kg/kg
  return(q)
}
