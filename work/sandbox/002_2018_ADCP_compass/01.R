library(oce)
pl <- oce.plot.ts
load('../../../adp.rda')
load('../../../pc.rda')
adpFull <- adp

focus <- as.POSIXct(c('2018-08-29 02:00:00', '2019-02-17 04:08:00'), tz='UTC')
adp <- subset(adp, focus[1] <= time & time <= focus[2])
lon <- -91.25105
lat <- 74.60635
t <- adp[['time']]
dec <- magneticField(rep(lon, length(t)), rep(lat, length(t)), t)$declination

hpc <- adp[['heading']]
hadp <- adp[['headingOriginal']] + dec
## clean up the ADCP heading to unwrap negatives and angles with large differences from the PC
hadp[hadp < 0] <- hadp[hadp < 0] + 360
II <- abs(hadp - hpc) > 180
hadp[II] <- hadp[II] + 360
II <- hpc > 360
hpc[II] <- hpc[II] - 360
hadp[II] <- hadp[II] - 360

## plot(hpc, hadp)

if (!interactive()) pdf('01.pdf')

oce.plot.ts(t, hpc)
lines(t, hadp, col=2)
legend('topleft', c('Pole Compass', 'ADCP'), lty=1, col=1:2)

oce.plot.ts(t, hpc,
            xlim=as.POSIXct(c('2018-12-01 00:00:00', '2018-12-15 00:00:00'), tz='UTC'))
lines(t, hadp, col=2)
legend('topleft', c('Pole Compass', 'ADCP'), lty=1, col=1:2)

hist(hadp, 100, col=2, main='', xlab='Heading')
hist(hpc, 100, add=TRUE, col=1)
legend('topleft', c('Pole Compass', 'ADCP'), lty=1, col=1:2)

plot(hpc, hadp,
     xlab='Pole Compass Heading',
     ylab='ADCP Heading')
hsp <- smooth.spline(hpc, hadp)
lines(predict(hsp), col=2, lwd=3)
grid()
abline(0, 1)

plot(hpc, hpc - hadp,
     xlab='Pole Compass Heading', ylab='ADCP Heading')
lines(approx(predict(hsp))$x, approx(predict(hsp))$x - approx(predict(hsp))$y, col=2, lwd=3)
grid()

hist(hpc - (hadp + dec), 100,
     main='', xlab=expression(Delta*Heading))

hsf <- function(x, hsp) {
    approx(hsp$y, hsp$x, x)$y
}

d <- subset(adpFull, focus[2] <= time & time <= as.POSIXct('2019-08-01'))
tt <- d[['time']]
hh <- d[['headingOriginal']] + magneticField(rep(lon, length(tt)), rep(lat, length(tt)), tt)$declination
hhh <- hsf(hh, hsp)
pl(t, hpc, xlim=c(focus[1], as.POSIXct('2019-08-01')))
lines(tt, hhh, col=2)

hist(hpc, 100, col=1, main='', xlab='Heading')
hist(hhh, 100, add=TRUE, col=2)
legend('topleft', c('Pole Compass', 'Pole compass fit'), lty=1, col=1:2)


adpFull[['heading']] <- c(hpc, hhh)
enu <- toEnu(adpFull)

plot(enu, which=1:2)

## plot(enu, which=19:20)

time <- enu[['time']]
e <- apply(enu[['v']][,,1], 1, mean, na.rm=TRUE)
n <- apply(enu[['v']][,,2], 1, mean, na.rm=TRUE)
## ef <- lowpass(e, n=25)
## nf <- lowpass(n, n=25)
ef <- approx(time, lowpass(e, n=25), time)$y
nf <- approx(time, lowpass(n, n=25), time)$y
par(mfrow=c(2, 1))
pl(time, e)
grid()
lines(time, ef, col=2, lwd=2)
pl(time, n)
grid()
lines(time, nf, col=2, lwd=2)
par(mfrow=c(1, 1))

par(mfrow=c(2, 1))
pl(time, e, xlim=as.POSIXct(c('2019-02-01', '2019-03-01'), tz='UTC'))
grid()
lines(time, ef, col=2, lwd=2)
abline(v=focus[2], lwd=2)
pl(time, n, xlim=as.POSIXct(c('2019-02-01', '2019-03-01'), tz='UTC'))
grid()
lines(time, nf, col=2, lwd=2)
abline(v=focus[2], lwd=2)
par(mfrow=c(1, 1))


if (!interactive()) dev.off()
