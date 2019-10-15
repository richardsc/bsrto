rm(list=ls())
library(oce)
library(curl)
dalftp <- FALSE

if (dalftp) {
    url <- 'ftp://dfoftp.ocean.dal.ca/pub/dfo/BSRTO/2019-2020/'
} else {
    url <- 'ftp://ftp.dfo-mpo.gc.ca/pittmanm/bsrto/2019-2020/'
}

dirs <- c('hpb/',
          'icl/',
          'imm/',
          'ips/',
          'lgH/',
          'mcA/',
          'mcH/',
          'mcI/',
          'pcm/',
          'rdi/')

savedir <- '/data/archive/barrow/2019/'

## First need to go through each directory to get a listing of the files
## We will exclude any files that have 0 size, as they will not result in a downloaded file and will retry every time the script is run
files <- list()
i <- 1
for (dir in dirs) {
    cat('* Reading', paste0(url, dir), '...')
    con <- curl(paste0(url, dir))
    tmp <- readLines(con)
    if (length(tmp) > 0) {
        if (dalftp) {
            nonzero <- read.table(text=tmp, stringsAsFactors=FALSE)$V5 != 0
            files[[i]] <- read.table(text=tmp, stringsAsFactors=FALSE)$V9[nonzero]
        } else {
            nonzero <- read.table(text=tmp, stringsAsFactors=FALSE)$V3 != 0
            files[[i]] <- read.table(text=tmp, stringsAsFactors=FALSE)$V4[nonzero]
        }
    } else {
        cat('  ** found no files in ', dir, '\n')
    }
    i <- i + 1
    close(con)
    cat('done\n')
}
names(files) <- dirs

## check for a local directory called bsrto, make it if not there
bsrto <- grep('bsrto', dir(savedir))
if (length(bsrto) < 1) {
    system(paste0('mkdir ', savedir, 'bsrto'))
}

## now check for each of the subdirs
for (dir in dirs) {
    subdir <- grep(sub('\\/$', '', dir), dir(paste0(savedir, 'bsrto')))
    if (length(subdir) < 1) {
        system(paste0('mkdir ', savedir, 'bsrto/', dir))
    }
}

## now get a list of all the files in each directory
existing_files <- list()
i <- 1
for (dir in dirs) {
    existing_files[[i]] <- dir(paste0(savedir, 'bsrto/', dir))
    i <-  i + 1
}
names(existing_files) <- dirs

## Now go through each directory and download any files that are not
## already downloaded
i <- 1
for (dir in dirs) {
    cat('* Checking', dir, '\n')
    files_to_get <- files[[i]][!(files[[i]] %in% existing_files[[i]])]
    cat('  ** Need to download', length(files_to_get), 'files\n')
    for (file in files_to_get) {
        cat('  ** Downloading', file, '\n')
        download.file(paste0(url, dir, file), destfile=paste0(savedir, 'bsrto/', dir, file))
    }
    i <- i + 1
}

##-----------------------
## Separate from the bsrto FTP site we want to download weather data
## from the Resolute Airport through Environment Canada
startYear <- 2019
startMonth <- 8
endYear <- as.POSIXlt(Sys.time())$year + 1900
endMonth <- as.POSIXlt(Sys.time())$mon + 1

if (startYear == endYear) {
    mon <- startMonth:endMonth
} else {
    yr <- startYear
    mon <- startMonth:12
    while (yr < endYear) {
        mon <- c(mon, 1:12)
        yr <- yr + 1
    }
    mon <- c(mon, 1:endMonth)
}
if (startYear == endYear) {
    year <- rep(startYear, length(mon))
} else {
    yr <- startYear
    year <- rep(startYear, length(startMonth:12))
    while (yr < endYear) {
        year <- c(year, rep(yr, 12))
        yr <- yr + 1
    }
    year <- c(year, rep(endYear, 12))
}

## Attempt to download the monthly csv files. Note, if a partial month
## file exists, it won't download, so we need to force it to
## re-download the current month everytime
destdir <- paste0(savedir, 'bsrto/met/')
metdir <- grep(sub('\\/$', '', 'met/'), dir(paste0(savedir, 'bsrto')))
if (length(metdir) < 1) {
    system(paste0('mkdir ', savedir, 'bsrto', '/met/'))
}

cat('* checking met data\n')
for (i in seq_along(mon)) {
    download.met(54199, year[i], mon[i], destdir=destdir, type='csv')
}

met_files <- dir(destdir, pattern='*.csv')
## Remove the last file and re-download
if (length(met_files) > 0) file.remove(paste0(destdir, tail(met_files, 1)))
for (i in seq_along(mon)) {
    download.met(54199, year[i], mon[i], destdir=destdir, type='csv')
}
