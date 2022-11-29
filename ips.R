rm(list=ls())
library(oce)

datadir <- '/data/archive/barrow/2022/bsrto/ips/'

files <- dir(datadir, full.names = TRUE)

## Notes:
## 
## * the latest ips data documentation says that the 4th entry of the
##   3rd line is barometric pressure, but actually it appears to be
##   the time-varying density. The barometricPressure has to come from
##   the shore station barometer (or from another source)

## read in the already-processed shore station barometer:
load('baro.rda')

### FIXME:
## One way to deal with the data processing/correction for
## atmospheric pressure is to do the processing here -- so that no
## corrections need to be applied downstream (e.g. inside the Shiny
## app). Fields that need to be corrected for the varying atmospheric pressure are:
## 
## * maxDraft
## * minDraft
## * meanDraft
## * range
## * rangeBin
## 
## For now I'll keep the processing simple, and do the corrections in
## the server.R code for when the plots are actually made

i <- 1
ips <- list()
files <- files[-grep('200223AA.bn4', files)]
for (file in files) {
    con <- file(file)
    dd <- readLines(con)
    close(con)
    ## find the spaces between 2 hour histograms. Should be the same
    ## everytime, but let's search it to be safe
    chunkIndex <- c(0, which(dd == '')) #add the first line, and drop the last
    for (ii in head(seq_along(chunkIndex), -1)) {
        d <- dd[(chunkIndex[ii]+1):(chunkIndex[ii+1])]
        time <- numberAsPOSIXct(unlist(strsplit(d[1], ' '))[1])
        if (time < as.POSIXct('2018-01-01', tz='UTC')) {
            time  <- as.POSIXct(paste(unlist(strsplit(d[1], ' '))[-(1:2)], collapse=' '),
                                format='%b %d %H:%M:%S %Y', tz='UTC')
        }
        tmp <- as.numeric(unlist(strsplit(d[3], ' ')))
        maxDraft <- tmp[1]
        minDraft <- tmp[2]
        meanDraft <- tmp[3]
        sdDraft <- tmp[4]
        tmp <- as.numeric(unlist(strsplit(d[4], ' ')))
        nRanges <- tmp[1]
        nPartialRanges <- tmp[2]
        soundSpeed <- tmp[3]
        ## barometricPressure <- tmp[4]
        density <- tmp[4]
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
        if (!any(is.na(bins))) {
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
            ##
            ## determine average barometric pressure for the 6 hour period from the
            ## shore station barometer
            II <- time <= baro$time & baro$time <= time + 6*3600
            barometricPressure <- mean(baro$patm[II], na.rm=TRUE)
            if (i > 1 & is.nan(barometricPressure)) barometricPressure <- ips[[i-1]]$barometricPressure # take the last value
            ## correct all the patm dependent fields
            
            ips[[i]] <- list(time=time, barometricPressure=barometricPressure,
                             density=density,
                             maxDraft=maxDraft, meanDraft=meanDraft,
                             minDraft=minDraft, sdDraft=sdDraft, maxPressure=maxPressure,
                             minPressure=minPressure, maxTemperature=maxTemperature,
                             minTemperature=minTemperature, maxPitch=maxPitch, maxRoll=maxRoll,
                             maxRollPitch=maxRollPitch, maxPitchRoll=maxPitchRoll,
                             rangeBin=rangeBin, rangeHist=rangeHist, range=range)
        }
        i <- i + 1
    }
}

save(file='ips.rda', ips)

