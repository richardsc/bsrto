rm(list=ls())
library(oce)

datadir <- '/data/archive/barrow/2017/bsrto/ips/'

files <- dir(datadir, full.names = TRUE)

i <- 1
ips <- list()
for (file in files) {
    con <- file(file)
    d <- readLines(con)
    close(con)
    time <- numberAsPOSIXct(unlist(strsplit(d[1], ' '))[1])
    tmp <- as.numeric(unlist(strsplit(d[3], ' ')))
    maxDraft <- tmp[1]
    minDraft <- tmp[2]
    meanDraft <- tmp[3]
    sdDraft <- tmp[4]
    tmp <- as.numeric(unlist(strsplit(d[4], ' ')))
    nRanges <- tmp[1]
    nPartialRanges <- tmp[2]
    soundSpeed <- tmp[3]
    barometricPressure <- tmp[4]
    gravity <- tmp[5]
    tmp <- as.numeric(unlist(strsplit(d[5], ' ')))
    maxPressure <- tmp[1]
    minPressure <- tmp[2]
    maxTemperature <- tmp[3]
    minTemperature <- tmp[4]
    tmp <- as.numeric(unlist(strsplit(d[6], ' ')))
    maxPitch <- tmp[1]
    maxRollPitch <- tmp[2]
    maxRoll <- tmp[3]
    maxPitchRoll <- tmp[4]
    maxInclination <- tmp[5]
    bins <- as.numeric(unlist(strsplit(d[7], ',')))
    minRange <- 9
    maxRange <- 22
    dRange <- 0.1
    rangeBin <- seq(9, 22-dRange, dRange)
    rangeHist <- bins[1:130]
    ## construct a time series according to the ranges
    range <- NULL
    for (ii in seq_along(rangeHist)) {
        range <- c(range, rep(rangeBin[ii], rangeHist[ii]))
    }
    ## FIXME: not yet including the above/below bin values
    ## correct for patm (should use Hub barometer)
    ## range <- range - barometricPressure/100
    ips[[i]] <- list(time=time, barometricPressure=barometricPressure,
                     maxDraft=maxDraft, meanDraft=meanDraft,
                     minDraft=minDraft, sdDraft=sdDraft, maxPressure=maxPressure,
                     minPressure=minPressure, maxTemperature=maxTemperature,
                     minTemperature=minTemperature, maxPitch=maxPitch, maxRoll=maxRoll,
                     maxRollPitch=maxRollPitch, maxPitchRoll=maxPitchRoll,
                     rangeBin=rangeBin, rangeHist=rangeHist, range=range)
    i <- i + 1
}
save(file='ips.rda', ips)

