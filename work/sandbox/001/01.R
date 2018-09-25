rm(list=ls())
library(oce)

load('../../../pc.rda')

if (!interactive()) pdf('01.pdf')

plot(pc$time, pc$heading)

hist(pc$heading, 100)

plot(pc$startTime, pc$meanHeading)

plot(pc$startTime, pc$sdHeading)

plot(pc$startTime, unlist(lapply(pcfull, function(x) x$heading[1])), main='First/last PC sample')
points(pc$startTime, unlist(lapply(pcfull, function(x) tail(x$heading, 1))), col=2)

if (!interactive()) dev.off()

if (!interactive()) pdf('01-ensembles.pdf')

jnk <- lapply(pcfull, function(x) plot(x$time, x$heading))

if (!interactive()) dev.off()

## How many points should be averaged to get a "good" heading (with low std)?
for (i in seq_along(pcfull)) {
    d <- pcfull[[i]]
    n <- length(d$time)
    pcfull[[i]]$avgLength <- 1:n
    avg <- sd <- NULL
    for (ii in 1:n) {
        avg[ii] <- mean(tail(d$heading, ii))
        sd[ii] <- sd(tail(d$heading, ii))
    }
    pcfull[[i]]$avg <- avg
    pcfull[[i]]$sd <- sd
}

n <- unlist(lapply(pcfull, function(x) x$avgLength))
sd <- unlist(lapply(pcfull, function(x) x$sd))

if (!interactive()) pdf('01-average.pdf')

plot(n, sd)
plot(n, sd, ylim=c(0, 5))

avg <- unlist(lapply(pcfull, function(x) x$avg - head(x$avg, 1)))

plot(n, avg, xlim=c(0, 20), ylab=expression(Delta*avg))
abline(h=c(-1, 1), lty=2)
plot(n, avg, xlim=c(0, 20), ylab=expression(Delta*avg), ylim=c(-5, 5))
abline(h=c(-1, 1), lty=2)

jnk <- lapply(pcfull, function(x) {
    plot(x$avgLength, x$avg - head(x$avg, 1))
    abline(h=c(-1, 1), lty=2)
    polygon(c(0, 0, 20, 20), c(-1, 1, 1, -1), col=rgb(0, 0, 0, 0.2))
})

if (!interactive()) dev.off()
