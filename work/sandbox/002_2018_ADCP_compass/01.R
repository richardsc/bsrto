library(oce)
load('../../../adp.rda')
load('../../../pc.rda')
pl <- oce.plot.ts

focus <- as.POSIXct(c('2018-08-29 02:00:00', '2019-02-17 04:08:00'), tz='UTC')
adpFull <- adp
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

plot(hpc, hadp - hpc,
     xlab='Pole Compass Heading', ylab='ADCP Heading')
##lines(approx(predict(hsp))$x, approx(predict(hsp))$x - approx(predict(hsp))$y, col=2, lwd=3)
lines(0:400, predict(hsp, 0:400)$y - 0:400, col=2, lwd=3)
grid()

hist((hadp + dec) - hpc, 100,
     main='', xlab=expression(Delta*Heading))


## To make a prediction of the heading based on the measured ADCP
## heading, we need a spline that is opposite to the above:
hsp2 <- smooth.spline(hadp, hpc)

## Predict heading for period after pole compass died
tf <- adpFull[['time']]
hadpf <- adpFull[['headingOriginal']] + magneticField(rep(lon, length(tf)), rep(lat, length(tf)),
                                                      tf)$declination
hnew <- predict(hsp2, hadpf)$y
pl(tf, hnew)
lines(t, hpc, col=2)

hist(hnew, 100)
hist(hpc, 100, col=2, add=TRUE)

hnew[hnew < 0] <- hnew[hnew < 0] + 360
hnew[hnew >= 360] <- hnew[hnew >= 360] - 360
II <- tf > focus[2]
adpFull[['heading']][II] <- hnew[II]

enu <- toEnu(adpFull)

plot(enu, which=19:20)

if (!interactive()) dev.off()
