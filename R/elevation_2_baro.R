#' @title Estimate barometric pressure from elevation
#'
#' @description
#' Calculate barometric pressure from elevation. Uses
#' approximat relationship based on XXX. See references.
#'
#' @param elevation Elevation in meters above sea level (MASL)
#'
#' @return Barometric pressure in millibars.
#'
#'
#' @export
elevation_2_baro = function(elevation){

  mmHg.inHg <- 25.3970886
  mmHg.mb <- 0.750061683
  standard.pressure.sea.level <- 29.92126
  standard.temperature.sea.level <- 15 + 273.15
  gravitational.acceleration <- 9.80665
  air.molar.mass <- 0.0289644
  universal.gas.constant <- 8.31447
  baro <- (1/mmHg.mb) * mmHg.inHg * standard.pressure.sea.level *
    exp((-gravitational.acceleration * air.molar.mass *
           elevation)/(universal.gas.constant * standard.temperature.sea.level))

  return(baro)
}
