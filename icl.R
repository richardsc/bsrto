rm(list=ls())
library(oce)

files <- dir('/data/archive/barrow/2017/bsrto/icl', full.names=TRUE)

if (!interactive()) {
    if (!('icl' %in% dir())) system('mkdir icl')
}

## read the icl/ dir to see how many images are there
run <- FALSE
nimage <- length(dir('icl'))
if (nimage == 0) {
    spec <- NULL
    time <- NULL
    icl <- list()
    run <- TRUE
} else if (nimage < length(files)) {
    load('icl.rda')
    run <- TRUE
} else if (nimage == length(files)) {
    print('No new files to process.')
} else {
    stop('More images than data files. Something is wrong!')
}

if (run) {
    ## for (i in (nimage+1):length(files)) {
    i <- nimage + 1
    for (f in files[(nimage+1):length(files)]) {
        cat('* Reading', f, '...')
        con <- file(f)
        h <- readLines(con, 29)
        close(con)
        startdate <- unlist(strsplit(h[grep('Start Date', h)], '\t'))[2]
        starttime <- unlist(strsplit(h[grep('Start Time', h)], '\t'))[2]
        d <- read.delim(f, stringsAsFactors=FALSE, skip=29)
        sdim <- c(length(d$Time), 410) # dim is always this size
        dd <- try(d[,7:416], silent=TRUE)
        if (inherits(dd, 'try-error')) {
            warning(paste('Corrupt spectrum detected on', paste(startdate, starttime)))
        } else if (is.character(as.matrix(dd))) {
            warning(paste('Corrupt spectrum detected on', paste(startdate, starttime)))
        } else {
            time <- c(time, as.POSIXct(paste(startdate, starttime), tz='UTC'))
            s <- as.matrix(dd)
            savg <- apply(s, 2, mean, na.rm=TRUE)
            spec <- rbind(spec, savg)
            ##t <- as.POSIXct(paste0(startdate, d$Time), tz='UTC')
            t <- tail(time, 1) + seq(0, length(d$Time))
            tn <- as.numeric(t) - as.numeric(t)[1]
            freq <- as.numeric(gsub('X', '', names(dd)))
            if (!interactive()) png(paste0('icl/icl-', sprintf('%04d', i), '.png'))
            par(mfrow=c(2, 1))
            imagep(tn, freq, s, xlab='Time [s]', ylab='Freq [Hz]', zlim=c(0, 50),
                   zlab=numberAsPOSIXct(time[i]),
                   col=oceColorsJet)
            matplot(freq, t(s), type='l', lty=1, col='lightgrey', xlab='Freq [Hz]',
                    ylab='Spectrum', ylim=c(0, 50))
            lines(freq, savg, lwd=3)    
            grid()
            if (!interactive()) dev.off()
            cat('done\n')
            icl[[i]] <- list(freq=freq, time=t, spec=s)
            i <- i + 1
        }
    }
    time <- numberAsPOSIXct(time)

    save(file='icl.rda', icl, spec, time, freq)
}
