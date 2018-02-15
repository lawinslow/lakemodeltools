library(ncdf4)

uwind = nc_open('z:/big_datasets/NLDAS/driver_ncdf4_NLDAS/UGRD10m_110_HTGL.nc4')
vwind = nc_open('z:/big_datasets/NLDAS/driver_ncdf4_NLDAS/VGRD10m_110_HTGL.nc4')


time = ncvar_get(uwind, 'time')
time = as.POSIXct(time, origin='1970-01-01', tz='UTC')

diff(range(which(time > as.POSIXct('1999-01-01') & time < as.POSIXct('2000-01-01'))))
range(which(time > as.POSIXct('2001-01-01') & time < as.POSIXct('2003-01-01')))

earlyall = list()
for(i in 1995:2000){
  starti = min(which(time > as.POSIXct(paste0(i, '-01-01'))))

  early = ncvar_get(uwind, 'UGRD10m_110_HTGL', start=c(1,1,starti), count=c(464,224,8757))
  earlyall[[i]] = apply(early, 1:2, mean)
  gc()
  cat(i, '\n')
}

early = apply(abind::abind(earlyall[1995:2000], along = 0), 2:3, mean)


lateall = list()
for(i in 2001:2006){
  starti = min(which(time > as.POSIXct(paste0(i, '-01-01'))))

  late = ncvar_get(uwind, 'UGRD10m_110_HTGL', start=c(1,1,starti), count=c(464,224,8757))
  lateall[[i]] = apply(late, 1:2, mean)
  gc()
}

late = apply(abind::abind(lateall[2001:2006], along = 0), 2:3, mean)


png('figures/step_change_before_after_2001.png', res=300,  width=2100, height=1800)
fields::image.plot(early-late)
dev.off()

