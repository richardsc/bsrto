rm(list=ls())
library(oce)
pl <- oce.plot.ts

## correct for altitude of Resolute barometric pressure measurements, formula from
## https://en.wikipedia.org/wiki/Atmospheric_pressure
## Resolute altitude is 68m
pcorr <- function(p0, h=68, g=9.80665, M=0.0289644, R0=8.31447, T0=288.15,
                  inverse=FALSE) {
    if (inverse) {
        p0 + p0*(1 - exp(-g*M*h/(R0*T0)))
    } else {
        p0*exp(-g*M*h/(R0*T0))
    }
}

datadir <- '/data/archive/barrow/2019/bsrto/met/'
files <- dir(datadir)

d <- list()
for (file in files) {
    d <- c(d, list=read.met(paste0(datadir, file)))
    if (exists('dd')) {
        dd <- rbind(dd, data.frame(tail(d, 1)[[1]]@data))
    } else {
        dd <- data.frame(tail(d, 1)[[1]]@data)
    }
}
met <- d[[1]]
met@data <- dd
## Note resolute data is on central standard (not DST) time, which is UTC-6
met[['time']] <- met[['time']] + 6*3600

## correct barometric pressure for altitude to get sea level pressure
met <- oceSetData(met, 'pressure', pcorr(met[['pressure']], inverse=TRUE))

save(file='met.rda', met)
