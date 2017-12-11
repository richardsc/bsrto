rm(list=ls())
library(oce)

findNearest <- function(x, value, na.val=-9999) {
    if (inherits(x, 'POSIXt')) x <- as.numeric(x); value <- as.numeric(value)
    na <- is.na(x)
    x[na] <- na.val
    out <- NULL
    for (i in 1:length(value)) {
        outtmp <- which(abs(x-value[i])==min(abs(x-value[i])))
        if (length(outtmp) > 1) outtmp <- outtmp[1] ## simple way to resolve ties
        out <- c(out, outtmp)
    }
    return(out)
}

dir <- '/data/archive/barrow/2017/bsrto/rdi/'

## Cat all the files together to make one complete one
system('cat /data/archive/barrow/2017/bsrto/rdi/*.rdi > adp.000')

adp <- read.oce('adp.000')

## use pole compass heading instead of ADCP compass by interpolation
load('pc.rda')
t <- adp[['time']]
pct <- pc$startTime
pch <- pc$meanHeading
h <- approx(pct, pch, t)$y

## find the correct pole compass heading by taking the nearest ensemble
hh <- NULL
for (i in seq_along(t)) {
    II <- findNearest(pct, t[i])
    hh[i] <- pch[II]
}

save(file='adp.rda', adp)
