rm(list=ls())
library(oce)
library(curl)

url <- 'ftp://ftp.dfo-mpo.gc.ca/pittmanm/bsrto/2017-2018/'

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

savedir <- '/data/archive/barrow/2017/'

## First need to go through each directory to get a listing of the files
## We will exclude any files that have 0 size, as they will not result in a downloaded file and will retry every time the script is run
files <- list()
i <- 1
for (dir in dirs) {
    cat('* Reading', paste0(url, dir), '...')
    con <- curl(paste0(url, dir))
    tmp <- readLines(con)
    nonzero <- read.table(text=tmp, stringsAsFactors=FALSE)$V3 != 0
    files[[i]] <- read.table(text=tmp, stringsAsFactors=FALSE)$V4[nonzero]
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
