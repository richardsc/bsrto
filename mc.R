rm(list=ls())
library(oce)
pl <- oce.plot.ts

datadir <- '/data/archive/barrow/2017/bsrto/'

inst <- c('mcH', 'mcA', 'imm', 'mcI')
mc_names <- list(mcH=c('temperature', 'conductivity', 'pressure', 'salinity', 'soundSpeed', 'date', 'time'),
                 mcA=c('temperature', 'conductivity', 'pressure', 'oxygen', 'salinity', 'soundSpeed', 'date', 'time', 'sample'),
                 imm=c('serialNumber', 'temperature', 'conductivity', 'pressure', 'date', 'time', 'sample'),
                 mcI=c('temperature', 'conductivity', 'pressure', 'salinity', 'soundSpeed', 'date', 'time'))

i <- 1
mc <- list()
for (m in inst) {
    system(paste0('cat ', datadir, m, '/* > ', m, '.mc'))
    d <- read.csv(paste0(m, '.mc'), col.names=mc_names[[i]], stringsAsFactors=FALSE)
    mc[[i]] <- as.ctd(temperature=d$temperature,
                      conductivity=d$conductivity,
                      pressure=d$pressure,
                      units = list(temperature=list(unit=expression(degree*C), scale='ITS-90'),
                                   conductivity=list(unit=expression(S/m), scale=''),
                                   pressure=list(unit=expression(dbar), scale='')),
                      deploymentType = 'moored')
    mc[[i]] <- oceSetData(mc[[i]], 'salinity', swSCTp(mc[[i]]))
    mc[[i]] <- oceSetData(mc[[i]], 'soundSpeed', swSoundSpeed(mc[[i]]))
    if ('oxygen' %in% names(d)) mc[[i]] <- oceSetData(mc[[i]], 'oxygen', d$oxygen)
    time <- as.POSIXct(paste(d$date, d$time), format='%d %b %Y %H:%M:%S', tz='UTC')
    mc[[i]] <- oceSetData(mc[[i]], 'time', time)
    i <- i + 1
}
names(mc) <- inst

save(file='mc.rda', mc)
