rm(list=ls())
library(oce)

datadir <- '/data/archive/barrow/2018/bsrto/pcm'

files <- dir(datadir, full.names = TRUE)

N <- 3 # number of points to average for the compass reading
pc <- list()
i <- 1
for (file in files) {
    sampleTime <- h <- tn <- m <- p <- NULL
    con <- file(file)
    l <- scan(con, character(), quiet=TRUE)
    close(con)
    if (length(l) > 5) { # must be at least 3 samples in the file
        startTime <- try(as.POSIXct(paste(l[1], l[2]), format='%m/%d/%Y %H:%M:%S', tz='UTC'),
                         silent=TRUE)
        if (inherits(startTime, 'try-error') | is.na(startTime)) {
            ## try to guess the start time from the filename
            date <- strsplit(tail(unlist(strsplit(file, '/')), 1), '.', fixed=TRUE)[[1]][1]
            startTime <- as.POSIXct(date, format='%y%m%d%H', tz='UTC')
        }
        h <- try(read.csv(text=l[3:length(l)], header=FALSE, stringsAsFactors=FALSE)[,2], silent=TRUE)
        if (inherits(h, 'try-error')) {
            pc[[i]] <- NULL
        } else {
            ## Sometimes there are headings that are exactly 0.0. For now
            ## let's just trim them out assuming they are erroneous.
            ok <- !is.na(h)
            h <- as.numeric(h[ok])
            h <- h[!(h==0)]
            sampleTime <- startTime + seq(0, length(h)-1)
            tn <- as.numeric(sampleTime) - as.numeric(sampleTime)[1]
            if (length(h) > 1) {
                m <- lm(h ~ tn)
                p <- summary(m)$coefficients[,4][[2]]
                pc[[i]] <- list(startTime=startTime, time=sampleTime, heading=h,
                                meanHeading=mean(tail(h, N), na.rm=TRUE), sdHeading=sd(tail(h, N), na.rm=TRUE),
                                trend=coef(m)[[2]], p=p)
            } else {
                pc[[i]] <- list(startTime=startTime, time=sampleTime, heading=h,
                                meanHeading=mean(tail(h, N), na.rm=TRUE), sdHeading=sd(tail(h, N), na.rm=TRUE))
            }
        }
    } else {
        pc[[i]] <- NULL
    }
    i <- i + 1
}
## remove any NULL entries (from corrupted files)
pc <- pc[unlist(lapply(pc, function(x) ifelse(is.null(x), FALSE, TRUE)))]
pcfull <- pc

pc <- list(time=numberAsPOSIXct(unlist(lapply(pc, function(x) x$time))),
           heading=unlist(lapply(pc, function(x) x$heading)),
           startTime=numberAsPOSIXct(unlist(lapply(pc, function(x) x$startTime))),
           meanHeading=unlist(lapply(pc, function(x) x$meanHeading)),
           sdHeading=unlist(lapply(pc, function(x) x$sdHeading)),
           trend=unlist(lapply(pc, function(x) x$trend)),
           p=unlist(lapply(pc, function(x) x$p)))

save(file='pc.rda', pc, pcfull)
