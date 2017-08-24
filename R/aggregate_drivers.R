#' @title Daily aggregate GLM drivers
#' @param glm_drivers data.frame of GLM formatted drivers
#'
#' @param avg.window Rolling window to apply first, in days.
#'
#'
#' @import fasttime
#' @import data.table
#' @importFrom zoo rollapply
#'
#' @export
driver_aggregate = function(glm_drivers, avg.window=1){


  daily = trunc(fasttime::fastPOSIXct(glm_drivers$time), units='days')
  glm_drivers$time = format(daily,'%Y-%m-%d %H:%M:%S')

  glm_drivers = data.table(glm_drivers)

  daily_drivers = glm_drivers[,.(ShortWave=mean(ShortWave),
                                  LongWave=mean(LongWave),
                                   AirTemp=mean(AirTemp),
                                   RelHum=mean(RelHum),
                                   WindSpeed=`^`(mean(`^`(WindSpeed,3)),1/3),
                                   Rain=mean(Rain),
                                   Snow=mean(Snow)), by=time]

  if(avg.window != 1){
    daily_drivers$ShortWave = c(rep(NA, avg.window-1), rollapply(daily_drivers$ShortWave, width=avg.window, align='left', FUN=mean))
    daily_drivers$LongWave = c(rep(NA, avg.window-1), rollapply(daily_drivers$LongWave, width=avg.window, align='left', FUN=mean))
    daily_drivers$AirTemp = c(rep(NA, avg.window-1), rollapply(daily_drivers$AirTemp, width=avg.window, align='left', FUN=mean))
    daily_drivers$RelHum = c(rep(NA, avg.window-1), rollapply(daily_drivers$RelHum, width=avg.window, align='left', FUN=mean))
    daily_drivers$WindSpeed = c(rep(NA, avg.window-1), rollapply(daily_drivers$WindSpeed, width=avg.window, align='left', FUN=mean))
    daily_drivers$Rain = c(rep(NA, avg.window-1), rollapply(daily_drivers$Rain, width=avg.window, align='left', FUN=mean))
    daily_drivers$Snow = c(rep(NA, avg.window-1), rollapply(daily_drivers$Snow, width=avg.window, align='left', FUN=mean))
  }

  return(as.data.frame(daily_drivers))
}
