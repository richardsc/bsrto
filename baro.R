rm(list=ls())
library(oce)

datadir <- '/data/archive/barrow/2019/bsrto/hpb/'

system(paste0('cat ', datadir, '* > baro.hpb'))

baro <- read.table('baro.hpb', stringsAsFactors = FALSE,
                col.names = c('date', 'time', 'patm', 'Tatm'))
baro$time <- as.POSIXct(paste(baro$date, baro$time), format='%m/%d/%Y %H:%M:%S', tz='UTC')
baro$patm <- baro$patm/10 # convert to kPa

save(file='baro.rda', baro)

