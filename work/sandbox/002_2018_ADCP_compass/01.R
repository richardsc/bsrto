library(oce)
load('../../../adp.rda')
load('../../../pc.rda')

focus <- as.POSIXct(c('2018-08-29 02:00:00', '2019-02-17 04:08:00'), tz='UTC')
adp <- subset(adp, focus[1] <= time & time <= focus[2])
lon <- -91.25105
lat <- 74.60635
t <- adp[['time']]
dec <- magneticField(rep(lon, length(t)), rep(lat, length(t)), t)$declination

if (!interactive()) pdf('01.pdf')

oce.plot.ts(t, adp[['heading']])
lines(t, adp[['headingOriginal']], col=2)
legend('topleft', c('Pole Compass', 'ADCP'), lty=1, col=1:2)

oce.plot.ts(t, adp[['heading']],
            xlim=as.POSIXct(c('2018-12-01 00:00:00', '2018-12-15 00:00:00'), tz='UTC'))
lines(t, adp[['headingOriginal']], col=2)
legend('topleft', c('Pole Compass', 'ADCP'), lty=1, col=1:2)

hist(adp[['headingOriginal']], 100, col=2, main='', xlab='Heading')
hist(adp[['heading']], 100, add=TRUE, col=1)
legend('topleft', c('Pole Compass', 'ADCP'), lty=1, col=1:2)

plot(adp[['heading']], adp[['headingOriginal']] + dec,
     xlab='Pole Compass Heading',
     ylab='ADCP Heading')
grid()
abline(0, 1)

plot(adp[['heading']], adp[['heading']] - (adp[['headingOriginal']] + dec),
     xlab='Pole Compass Heading', ylab='ADCP Heading')
grid()

hist(adp[['heading']] - (adp[['headingOriginal']] + dec), 100,
     main='', xlab=expression(Delta*Heading))

if (!interactive()) dev.off()
