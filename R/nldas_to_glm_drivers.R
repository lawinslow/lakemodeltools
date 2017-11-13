#' @title Convert NLDAS to GLM drivers
#'
#' @param drop.precip Set precip to zero temp is below freezing (and precip is converted to snow)
#'
#'
#' @export
nldas_to_glm_drivers = function(driver_df, drop.precip=TRUE){

  #fix up the col names, differ a little depending on source
  if(any(grepl('_110_SFC', names(driver_df), ignore.case = TRUE))){
    names(driver_df) = tolower(sapply(strsplit(names(driver_df), '_'), function(x)x[1]))
  }




  ## convert and downsample wind
  driver_df$ShortWave = driver_df$dswrfsfc
  driver_df$LongWave  = driver_df$dlwrfsfc

  if('windspeed' %in% names(driver_df)){
    driver_df$WindSpeed = driver_df$windspeed
  }else if('ugrd10m' %in% names(driver_df)){
    driver_df$WindSpeed = sqrt(driver_df$ugrd10m^2 + driver_df$vgrd10m^2)
  }else{
    stop('Unable to find wind data.\nDriver service must have temp data (named windspeed or ugrd10m). ')
  }



  ##TODO Maybe: Generalize these conversions so they aren't if/else statements
  if('tmp2m' %in% names(driver_df)){
    driver_df$AirTemp   = driver_df$tmp2m - 273.15 #convert K to deg C
  }else if('airtemp' %in% names(driver_df)){
    driver_df$AirTemp   = driver_df$airtemp #no conversion neede
  }else{
    stop('Unable to find temperature data.\nDriver service must have temp data (named tmp2m or airtemp). ')
  }

  if('relhum' %in% names(driver_df)){
    driver_df$RelHum    = 100*driver_df$relhum
  }else if('spfh2m' %in% names(driver_df)){
    driver_df$RelHum    = 100*driver_df$spfh2m/qsat(driver_df$tmp2m-273.15, driver_df$pressfc*0.01)
  }else if('relhumperc' %in% names(driver_df)){
    driver_df$RelHum    = driver_df$relhumperc
  }else{
    stop('Unable to find humidity data.\nDriver service must have humidity data (named relhum or spfh2m). ')
  }

  if('apcpsfc' %in% names(driver_df)){
    #convert from mm/hour to m/day
    driver_df$Rain      = driver_df$apcpsfc*24/1000 #convert to m/day rate
  }else if('precip' %in% names(driver_df)){
    #convert from mm/day to m/day
    driver_df$Rain      = driver_df$precip/1000
  }else if('pr' %in% names(driver_df)){
    #convert from kg/m^2/sec
    driver_df$Rain      = driver_df$pr * (60*60*24)/1e3  #convert to m/day rate
  }else{
    stop('Unable to find precipitation data. \nMust be either apcpsfc, precip, or pr')
  }


  #now deal with snow base case
  driver_df$Snow = 0

  # 10:1 ratio assuming 1:10 density ratio water weight
  driver_df$Snow[driver_df$AirTemp < 0] = driver_df$Rain[driver_df$AirTemp < 0]*10

  if(drop.precip){
    driver_df$Rain[driver_df$AirTemp < 0] = 0
  }


  #convert DateTime to properly formatted string
  # and check datetime name format
  if(any(names(driver_df) == 'datetime')){
    driver_df$DateTime = driver_df$datetime
  }
  driver_df$time = driver_df$DateTime
  driver_df$time = format(driver_df$time,'%Y-%m-%d %H:%M:%S')

  return(driver_df[order(driver_df$time), c('time','ShortWave','LongWave','AirTemp','RelHum','WindSpeed','Rain','Snow')])
}
