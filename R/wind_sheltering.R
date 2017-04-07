#' @title Calculate wind sheltering coefficient
#'
#' @description
#' Calculate the wind sheltering coefficient of a lake.
#'
#' @param area Lake area in meters^2 (square meters)
#' @param canopy Surrounding canopy height in meters (for 'Markfort' method)
#' @param method The desired calculation method one of c('Markfort', 'Hondzo')
#'
#' @return
#' The wind sheltering coefficient (between 0 and 1)
#' @details
#'
#' @references
#' CD Markfort, Perez AL, Thill JW, Jaster DA, Porté-Agel F, Stefan HG,
#' 2010. Wind sheltering of a lake by a tree canopy or bluff topography. Water Resour. Res. 46.
#'
#' M Hondzo, Stefan HG, 1993. Lake water temperature simulation model. J. Hydraulic Eng. 119, 1251–1273
#'
#'
#'
#'
#'
#'@export
wind_sheltering <- function(area, canopy=NA, method='Hondzo'){

  lkeArea	<-	area

  if(is.na(lkeArea)){
    return(NA)
  }

  if (method=='Markfort'){
    # Markfort et al. 2010
    minWstr	<-	0.0001

    hc	<-	canopy


    if(is.na(hc) | is.null(hc)){
      return(NA)
    }

    Xt	<-	50*hc

    D	<-	2*sqrt(lkeArea/pi)
    if (D<Xt){
      wind.shelter	<-	minWstr
    } else {
      wind.shelter	<-	2/pi*acos(Xt/D)-(2*Xt/(pi*D^2))*sqrt(D^2-Xt^2)
    }
  } else if (method=="Hondzo") {
    lkeArea.km2 = lkeArea*1.0e-6 # to km2
    wind.shelter= 1.0 - exp(-0.3*lkeArea.km2)
  } else {
    # Markfort et al. 2010
    minWstr	<-	0.0001
    if (is.null(canopy)){
      hc	<-	max(c(getCanopy(site_id),1))
    } else {
      hc	<-	canopy
    }

    Xt	<-	50*hc

    perim	<-	getPerim(site_id)
    shelArea	<-	perim*hc*12.5 # 25% of Markfort, as sheltering is single direction...
    shelter	<-	(lkeArea-shelArea)/lkeArea
    wind.shelter	<-	max(c(shelter,minWstr))
    if (is.null(perim)){wind.shelter<-NULL}
  }

  return(wind.shelter)
}
