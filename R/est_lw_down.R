#' @title Estimate downwelling longwave radiation
#'
#' @description
#' Estimate downwelling longwave radiation based on atmospheric conditions.
#'
#' @export
est_lw_down <- function(dateTime,sw,lat,atm.press,airT,RH){

  if(!inherits(dateTime, "POSIXt")){
    stop('dateTime supplied to calc.lw.net must be of POSIXct class. See ?as.POSIXct')
  }

  # estimate clear sky short wave radiation
  clearsky <- calc.clearsky(dateTime,lat,atm.press,airT,RH)

  # estimate cloud cover fraction from observed and clear sky short wave radiation
  clf <- 1 - (sw/clearsky)
  clf[sw == 0] <- 0
  clf[clf<0] <- 0
  clf[clf == Inf] <- 0
  clf[clf>1] <- 1

  # determine month of year
  month <- as.POSIXlt(dateTime)$mon + 1 #extract month


  es <- 1000*SatVaporFromTemp(airT)
  vp <- (RH*0.01*es)

  # estimate incoming longwave radiation
  T_k <- 273.13 + airT
  cl1 <- 1-clf
  cl2 <- 1.22+0.06*sin((month+2)*pi/6)
  cl3 <- vp/T_k
  T_k1 <- T_k^4
  S_B <- 5.67E-8 # Stefan-Boltzman constant (K is used)
  LWin <- T_k1*(clf+cl1*cl2*cl3^(1/7))*S_B

  # estimate outgoing longwave radiation
  # Tks <- Ts + 273.13
  # emiss <- 0.972
  # LWo <- S_B*emiss*Tks^4
  #lwnet <- LWin-LWo

  return(as.numeric(LWin))
}

calc.clearsky <- function(dateTime,lat,atm.press,airT,RH){

  #if inputs are not defined
  #assume RH=%, convert to fraction
  if (missing(atm.press)){atm.press <- 1013}
  if (missing(airT)){airT <- 20}
  if (missing(RH)){RH <- 0.7}else{RH <- RH*0.01}

  #resize vectors
  if(length(dateTime)>length(lat)) {
    lat.vec <- rep(1,length(dateTime))*lat
  } else {
    lat.vec <- matrix(lat,length(lat),1)
  }
  if(length(dateTime)>length(atm.press)) {
    atm.press <- rep(1,length(dateTime))*atm.press
  } else {
    atm.press <- matrix(atm.press,length(atm.press),1)
  }
  if(length(dateTime)>length(airT)) {
    airT <- rep(1,length(dateTime))*airT
  } else {
    airT <- matrix(airT,length(airT),1)
  }
  if(length(dateTime)>length(RH)) {
    RH <- rep(1,length(dateTime))*RH
  } else {
    RH <- matrix(RH,length(RH),1)
  }

  t_noon = 12.5;          #solar noon (actually will depend on timezone)
  n = as.POSIXlt(dateTime)$yday+1 #julian day
  t = as.POSIXlt(dateTime)$hour # time (hours)

  cosN = 1+0.034*cos(2*pi*(n-1)/365)
  I0 = 1353*(cosN^2)    # Meyers & Dale 1983

  H = (pi/12)*(t_noon-t)   # t_noon is solar noon, t is the local solar time
  #eg) t = 12.5 and H = 0 at local solar noon

  sigma = 180/pi*(0.006918-.399912*cos(2*pi/365*(n-1))+0.070257*
                    sin(2*pi/365*(n-1))-.006758*cos(4*pi/365*(n-1))+
                    0.000907*sin(4*pi/365*(n-1))-0.002697*cos(6*pi/365*(n-1))+
                    0.00148*sin(6*pi/365*(n-1)))


  #- - cosine of solar zenith angle - -
  sin1 = sin(lat.vec*2*pi/360);
  sin2 = sin(sigma*2*pi/360);
  cos1 = cos(lat.vec*2*pi/360);
  cos2 = cos(sigma*2*pi/360);
  cos3 = cos(H);
  cosZ = sin1*sin2+cos1*cos2*cos3

  p=atm.press #in millibars
  m1=1224*cosZ*cosZ+1
  m=35*m1^(-0.5) #optical air mass at p=1013 mb
  Tr1 = m*(0.000949*p+0.051)
  T_rT_pg = 1.021-0.084*Tr1^(0.5)      #Atwater & Brown 1974

  Es = 1000*SatVaporFromTemp(airT)  #modified Clausius Clapeyron Equation (0.7% @ -40C to 0.006% @ 40C)
  E  = (RH*Es)
  Td1 = 243.5*log(E/6.112)
  Td2 = 17.67-log(E/6.112)
  T_d = Td1/Td2                     # dewpoint (degrees c)
  T_d = T_d*9/5+32                   # dewpoint (degrees F)
  G = GetSmithGamma(lat,dateTime) # empirical constant dependent upon time of the year and latitude (Smith 1966 table 1)

  T_a = m
  for (i in 1:length(m)) {
    T_a[i] = 0.935^m[i]            # Meyers & Dale 1993
  }

  mu = exp(0.1133-log(G+1)+0.0393*T_d) # precipitable water

  mu1 = mu*m
  T_w = 1-.077*mu1^0.3

  ISW = I0*cosZ*T_rT_pg*T_w*T_a
  useI = (ISW>0)+0
  clrSW = useI*ISW

  return(clrSW)
}

SatVaporFromTemp <- function(airT) {

  Kelvin = 273.15
  SatVapor=rep(1:length(airT))

  for (i in 1:length(SatVapor)){
    temp=exp(53.67957 - (6743.769/(airT[i]+Kelvin))-(4.8451*log(airT[i]+Kelvin)))
    SatVapor[i]=temp
  }

  SatVapor = SatVapor/1000

  return(SatVapor)
}


GetSmithGamma <- function(latitude,dateTime){

  #Table 1 from Smith 1966
  #Note on the relationship between total precipitable water and surface dewpoint. J. Appl. Meteor.,5, 726-727.
  gammatable=matrix(c(3.37,2.85,2.80,2.64,2.99,3.02,2.70,2.93,
                      3.60,3.00,2.98,2.93,3.04,3.11,2.92,2.94,
                      2.70,2.95,2.77,2.71,2.52,3.07,2.67,2.93,
                      1.76,2.69,2.61,2.61,1.60,1.67,2.24,2.63,
                      1.11,1.44,1.94,2.02),
                    9,4,byrow=TRUE)
  #colnames(gammatable)=c("winter","spring","summer","fall")

  latitude=ceiling(latitude/10)
  n = as.POSIXlt(dateTime)$yday+1 #julian day

  season = rep(1,length(n))
  for (i in 1:length(season)) {
    if (n[i]>=80 & n[i]<172) {
      season[i] =2 #spring
    } else if (n[i]>=172 & n[i]<266) {
      season[i] =3 #summer
    } else if (n[i]>=266 & n[i]<355) {
      season[i] =4 #fall
    } else {
      season[i] =1 #winter
    }
  }

  gamma = rep(1,length(season))
  latitude= rep(latitude,length(season))  #whoa, this is exploding in size, changed code above
  for (i in 1:length(gamma)) {
    gamma[i] = gammatable[latitude[i],season[i]]
  }

  return(gamma)
}

