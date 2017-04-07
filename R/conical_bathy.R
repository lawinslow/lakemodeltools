#' @title Estimate conical bathymetry
#'
#' @description
#' Creates simple conical bathymetry profile based
#' on lake area and max observed depth. Useful
#' in situations where true hypsometry or
#' bathymetry is unknown.
#'
#' @param area Lake area in desired units (usually meters^2)
#' @param zmax Maximum lake depth (usually in meters)
#'
#'
#'
#' @export
conical_bathy = function(area, zmax){

  numZ	    =	15
  topradius = sqrt(area/pi)
  depth	    = seq(0,zmax,length.out=numZ)
  radii     = approx(c(0,zmax),c(topradius,0),depth)$y
  area      = pi*radii^2
  bathymetry= data.frame(depths=depth,areas=area)

  return(bathymetry)
}
