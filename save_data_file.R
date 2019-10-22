## This script loads the various data files from the different
## intstruments, and re-saves it into a new RDS object, for use with
## reactiveDataReader in the Shiny app
library(oce)

load('mc.rda')
load('met.rda')
load('baro.rda')
load('ips.rda')
load('icl.rda')
load('adp.rda')
ipsTime <- numberAsPOSIXct(unlist(lapply(ips, function(x) x[['time']])))
maxDraft <- unlist(lapply(ips, function(x) x[['maxDraft']]))
meanDraft <- unlist(lapply(ips, function(x) x[['meanDraft']]))
pAtm <- unlist(lapply(ips, function(x) x[['barometricPressure']]))/10

saveRDS(list(mc=mc, met=met, baro=baro, ips=ips, icl=icl,
             adp=adp, enu=enu, time=time, freq=freq, spec=spec,
             ipsTime=ipsTime, maxDraft=maxDraft,
             meanDraft=meanDraft, pAtm=pAtm),
        file='data.rds')
