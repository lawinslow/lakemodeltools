#' @title Coefficient of wind drag
#' @description
#' Get coefficient of wind drag for a lake with a supplied wind sheltering coefficient
#'
#' @param site_id The character ID for the requested data
#' @param wstr The wind sheltering coefficient (see \link{wind_sheltering})
#'
#' @return
#' Coefficient of wind drag
#'
#' @details
#' TODO
#'
#'
#'
#'
#'
#'@export
coef_drag <-	function(wstr=NULL){

  coef_wind_drag.ref	<-	0.00140
  coef_wind_drag	<-	coef_wind_drag.ref*wstr^0.33
  return(coef_wind_drag)
}
