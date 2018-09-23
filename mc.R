rm(list=ls())
library(oce)
pl <- oce.plot.ts

datadir <- '/data/archive/barrow/2018/bsrto/'

inst <- c('mcH', 'mcA', 'imm', 'mcI')
mc_names <- list(mcH=c('temperature', 'conductivity', 'pressure', 'salinity', 'soundSpeed', 'date', 'time'),
                 ## mcA=c('temperature', 'conductivity', 'pressure', 'oxygen', 'salinity', 'soundSpeed', 'date', 'time', 'sample'), #2017
                 mcA=c('temperature', 'conductivity', 'pressure', 'oxygen', 'salinity', 'soundSpeed', 'date', 'time'), #2018
                 imm=c('serialNumber', 'temperature', 'conductivity', 'pressure', 'date', 'time', 'sample'),
                 ## mcI=c('temperature', 'conductivity', 'pressure', 'salinity', 'soundSpeed', 'date', 'time')) # 2017
                 mcI=c('temperature', 'conductivity', 'pressure', 'oxygen', 'salinity', 'soundSpeed', 'date', 'time')) # 2018

i <- 1
mc <- list()
for (m in inst) {
    system(paste0('cat ', datadir, m, '/* > ', m, '.mc'))

    ## sometimes garbage characters get inserted at the beginning of
    ## the line for the imm microcat (see Dec 29 2017 and Jan 4
    ## 2018). To guard against this we run the file through a system
    ## `tr` command
    if (m == 'imm') {
        system("tr -cd '\11\12\15\40-\176' < imm.mc > imm_clean.mc")
        system("mv imm.mc imm_old.mc")
        system("mv imm_clean.mc imm.mc")
    }
    if (m == 'mcI') {
        con <- file('mcI.mc')
        dd <- readLines(con)
        close(con)
        ## Need to remove the "start sample", "start time", and empty lines (groups of 3)
        II <- grep('start sample', dd)
        III <- NULL
        for (ii in seq_along(II)) {
            III <- c(III, II[ii], II[ii]+1, II[ii]+2)
        }
        dd <- dd[-III]
        dd <- read.csv(text=dd, col.names=mc_names[[i]], stringsAsFactors=FALSE)
    } else {
        dd <- read.csv(paste0(m, '.mc'), col.names=mc_names[[i]], stringsAsFactors=FALSE)
    }
    time <- as.POSIXct(paste(dd$date, dd$time), format='%d %b %Y %H:%M:%S', tz='UTC')
    ## any with messed up times? (e.g. sample 544 from imm)
    bad <- is.na(time)
    d <- dd[!bad,]
    time <- as.POSIXct(paste(d$date, d$time), format='%d %b %Y %H:%M:%S', tz='UTC')
    d <- d[order(as.numeric(time)),]
    mc[[i]] <- as.ctd(temperature=as.numeric(d$temperature),
                      conductivity=as.numeric(d$conductivity),
                      pressure=as.numeric(d$pressure),
                      units = list(temperature=list(unit=expression(degree*C), scale='ITS-90'),
                                   conductivity=list(unit=expression(S/m), scale=''),
                                   pressure=list(unit=expression(dbar), scale='')),
                      deploymentType = 'moored')
    mc[[i]] <- oceSetData(mc[[i]], 'salinity', swSCTp(mc[[i]]))
    ## do some sanity checks on the data
    mc[[i]][['temperature']][mc[[i]][['temperature']] < -2 | mc[[i]][['temperature']] > 30] <- NA
    mc[[i]][['salinity']][mc[[i]][['salinity']] < 1 | mc[[i]][['salinity']] > 40] <- NA
    mc[[i]] <- oceSetData(mc[[i]], 'soundSpeed', swSoundSpeed(mc[[i]]))
    if ('oxygen' %in% names(d)) mc[[i]] <- oceSetData(mc[[i]], 'oxygen', d$oxygen)
    time <- as.POSIXct(paste(d$date, d$time), format='%d %b %Y %H:%M:%S', tz='UTC')
    mc[[i]] <- oceSetData(mc[[i]], 'time', time)
    i <- i + 1
}
names(mc) <- inst

save(file='mc.rda', mc)
