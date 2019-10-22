library(shiny)
library(oce)
d <- readRDS('data.rds')

mcplot <- function(x, field, xlim, pch=21, col, add=FALSE) {
    if (length(x) > 1) {
        ## if (missing(xlim)) xlim <- range(x[[1]][['time']])
        if (missing(xlim)) xlim <- c(min(unlist(lapply(x, function(y) min(y[['time']], na.rm=TRUE)))),
                                     max(unlist(lapply(x, function(y) max(y[['time']], na.rm=TRUE)))))
        if (field == "T/S") {
            Srange <- range(unlist(lapply(x, function(x) x[['salinity']])), na.rm=TRUE)
            Trange <- range(unlist(lapply(x, function(x) x[['temperature']])), na.rm=TRUE)
            for (i in 1:length(x)) {
                if (i == 1) {
                    plotTS(x[[i]], cex=1.5, pch=21, pt.bg=ifelse(missing(col), i, col), Tlim=Trange, Slim=Srange)
                } else {
                    plotTS(x[[i]], cex=1.5, pch=21, pt.bg=ifelse(missing(col), i, col), add=TRUE)
                }
            }
        } else {
            range <- range(unlist(lapply(x, function(x) x[[field]])), na.rm=TRUE)
            for (i in 1:length(x)) {
                if (i == 1) {
                    oce.plot.ts(x[[i]][['time']], x[[i]][[field]], ylab=field, ylim=range, xlim=xlim)
                } else {
                    lines(x[[i]][['time']], x[[i]][[field]], col=ifelse(missing(col), i, col))
                }
            }
        }
    } else {
        x <- x[[1]]
        if (missing(xlim)) xlim <- range(x[['time']])
        if (field == "T/S") {
            plotTS(x, cex=1.5, pch=pch, pt.bg=ifelse(missing(col), 'lightgrey', col), add=add)
        } else {
            if (field == 'oxygen' & !('oxygen' %in% names(x@data))) {
                plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
                box()
                text(0.5, 0.5, "No oxygen data available at this depth")
            } else {
                oce.plot.ts(x[['time']], x[[field]], ylab=field, xlim=xlim)
                grid()
            }
        }
    }
}

findNearest <- function(x, n) {
    x <- as.numeric(x)
    n <- as.numeric(n)
    which(abs(x-n)==min(abs(x-n)))
}

whichmc <- function(input) {
    res <- if (input == "40m") {
               "mcI"
           } else if (input == "60m") {
               "mcA"
           } else if (input == "80m") {
               "imm"
           } else if (input == "160m") {
               "mcH"
           } else if (input == "All") {
               1:4
           }
    return(res)
}

shinyServer(function(input, output, session) {
    state <- reactiveValues()

    data <- reactiveFileReader(60000,
                               session,
                               'data.rds',
                               readRDS)
    
    output$plot <- renderPlot({
        d <- data()
        if (!is.null(input$select)) {
            if (input$select == 1) {
                if (is.null(state$xlim)) {
                    mcplot(d$mc[whichmc(input$mc)], input$field)
                } else {
                    mcplot(d$mc[whichmc(input$mc)], input$field, xlim=state$xlim)
                }
            } else if (input$select == 2) {
                if (is.null(state$xlim)) {
                    imagep(d$time, d$freq, d$spec, ylab='Frequency [Hz]', col=oceColorsViridis,
                           zlim=c(0, 100), decimate=FALSE)
                } else {
                    imagep(d$time, d$freq, d$spec, ylab='Frequency [Hz]', col=oceColorsViridis,
                           zlim=c(0, 100), xlim=state$xlim, decimate=FALSE)
                }
            } else if (input$select == 3) {
                if (is.null(state$xlim)) {
                    oce.plot.ts(d$ipsTime, d$maxDraft-d$pAtm, type='b', pch=3,
                                ylim=c(-1, max(d$maxDraft-d$pAtm, na.rm=TRUE)),
                                ylab='Ice draft [m]')
                    grid()
                    legend('topleft', c('Maximum Draft', 'Mean Draft'), pch=c(3, 1))
                    points(d$ipsTime, d$meanDraft-d$pAtm)
                } else {
                    oce.plot.ts(d$ipsTime, d$maxDraft-d$pAtm, type='b', pch=3,
                                ylab='Ice draft [m]',
                                ylim=c(-1, max(d$maxDraft-d$pAtm, na.rm=TRUE)),
                                xlim=state$xlim)
                    grid()
                    legend('topleft', c('Maximum Draft', 'Mean Draft'), pch=c(3, 1))
                    points(d$ipsTime, d$meanDraft-d$pAtm)
                }
            } else if (input$select == 4) {
                if (is.null(state$xlim)) {
                    if (input$adp == "U") {
                        plot(d$enu, which=1)
                    } else if (input$adp == "V") {
                        plot(d$enu, which=2)
                    } else if (input$adp == "depth-averaged U") {
                        plot(d$enu, which=19)
                    } else if (input$adp == "depth-averaged V") {
                        plot(d$enu, which=20)
                    } else if (input$adp == "backscatter1") {
                        plot(d$enu, which=5)
                    } else if (input$adp == "backscatter2") {
                        plot(d$enu, which=6)
                    } else if (input$adp == "backscatter3") {
                        plot(d$enu, which=7)
                    } else if (input$adp == "backscatter4") {
                        plot(d$enu, which=8)
                    } else if (input$adp == "average backscatter") {
                        bsavg <- apply(beamUnspreadAdp(d$enu)[['a', 'numeric']], 1, mean, na.rm=TRUE)
                        oce.plot.ts(d$enu[['time']], bsavg, ylab='Average backscatter')
                    } else {
                        plot(d$enu, which=input$adp)
                    }
                } else {
                    if (input$adp == "U") {
                        plot(d$enu, which=1, xlim=state$xlim)
                    } else if (input$adp == "V") {
                        plot(d$enu, which=2, xlim=state$xlim)
                    } else if (input$adp == "depth-averaged U") {
                        plot(d$enu, which=19, xlim=state$xlim)
                    } else if (input$adp == "depth-averaged V") {
                        plot(d$enu, which=20, xlim=state$xlim)
                    } else if (input$adp == "backscatter1") {
                        plot(d$enu, which=5, xlim=state$xlim)
                    } else if (input$adp == "backscatter2") {
                        plot(d$enu, which=6, xlim=state$xlim)
                    } else if (input$adp == "backscatter3") {
                        plot(d$enu, which=7, xlim=state$xlim)
                    } else if (input$adp == "backscatter4") {
                        plot(d$enu, which=8, xlim=state$xlim)
                    } else if (input$adp == "average backscatter") {
                        bsavg <- apply(beamUnspreadAdp(d$enu)[['a', 'numeric']], 1, mean, na.rm=TRUE)
                        oce.plot.ts(d$enu[['time']], bsavg, ylab='Average backscatter', xlim=state$xlim)
                    } else {
                        plot(d$enu, which=input$adp, xlim=state$xlim)
                    }
                }
            } else if (input$select == 5) {
                if (is.null(state$xlim)) {
                    if (input$baro == "pressure") {
                        oce.plot.ts(d$baro$time, d$baro$patm, ylab='Barometric Pressure [kPa]')
                    } else if (input$baro == "temperature") {
                        oce.plot.ts(d$baro$time, d$baro$Tatm, ylab='HUB temperature')
                    }
                    grid()
                } else {
                    if (input$baro == "pressure") {
                        oce.plot.ts(d$baro$time, d$baro$patm, ylab='Barometric Pressure [kPa]',
                                    xlim=state$xlim)
                    } else if (input$baro == "temperature") {
                        oce.plot.ts(d$baro$time, d$baro$Tatm, ylab='HUB temperature',
                                    xlim=state$xlim)
                    }
                    grid()
                }
            } else if (input$select == 6) {
                cm <- colormap(lowpass(d$met[['speed']], n=15), col=oceColorsViridis,
                               zlim=c(0, max(d$met[['speed']], na.rm=TRUE)))
                if (input$met == 'humidity') {
                    ylim <- c(0, 100)
                } else {
                    ylim <- range(d$met[[input$met]], na.rm=TRUE)
                }
                if (is.null(state$xlim)) {
                    if (input$met == "stickPlot") {
                        II <- seq(1, length(d$met[['time']]), 2)
                        drawPalette(colormap=cm)
                        plotSticks(d$met[['time']][II], 0,
                                   lowpass(d$met[['u']], n=15)[II],
                                   lowpass(d$met[['v']], n=15)[II],
                                   yscale=20,
                                   yaxt='n', col=cm$zcol[II])
                        oce.axis.POSIXct(1)
                        grid()
                        arrows(par('usr')[1] + 0.05*(diff(par('usr')[1:2])),
                               -1,
                               par('usr')[1] + 0.05*(diff(par('usr')[1:2])),
                               -1 + 10/20,
                               len=1/20, col=2, lwd=2)
                        text(par('usr')[1] + 0.05*(diff(par('usr')[1:2])), -1 + 0.5*10/20,
                             ' 10 m/s', col=2, adj=0)
                    } else {
                        oce.plot.ts(d$met[['time']], d$met[[input$met]],
                                    ylab=d$met[['units']][[input$met]]$unit,
                                    ylim=ylim)
                        grid()
                    }
                } else {
                    if (input$met == "stickPlot") {
                        plotSticks(d$met[['time']], 0,
                                   lowpass(d$met[['u']], n=15),
                                   lowpass(d$met[['v']], n=15),
                                   yscale=20,
                                   yaxt='n',
                                   xlim=state$xlim,
                                   col=cm$zcol)
                        oce.axis.POSIXct(1)
                        grid()
                        arrows(par('usr')[1] + 0.05*(diff(par('usr')[1:2])),
                               -1,
                               par('usr')[1] + 0.05*(diff(par('usr')[1:2])),
                               -1 + 10/20,
                               len=1/20, col=2, lwd=2)
                        text(par('usr')[1] + 0.05*(diff(par('usr')[1:2])), -1 + 0.5*10/20,
                             ' 10 m/s', col=2, adj=0)
                    } else {
                        oce.plot.ts(d$met[['time']], d$met[[input$met]],
                                    ylab=d$met[['units']][[input$met]]$unit,
                                    xlim=state$xlim, ylim=ylim)
                        grid()
                    }
                }
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, "Please choose a data type to display")
        }
    })

    output$plot2 <- renderPlot( {
        d <- data()
        if (input$select == 1) {
            if (input$mc == "All") {
                mcplot(d$mc, "T/S")
            } else {
                if (is.null(state$brushed)) {
                    mcplot(d$mc, col='lightgrey', "T/S")
                    mcplot(d$mc[whichmc(input$mc)], "T/S", col=2, add=TRUE)
                } else {
                    mcplot(d$mc, col='lightgrey', "T/S")
                    if (input$mc == "All") {
                        for (i in 1:length(d$mc)) {
                            tmp <- list(subset(d$mc[[i]], state$brushed))
                        }
                    } else {
                        tmp <- list(subset(d$mc[[whichmc(input$mc)]], state$brushed))
                        mcplot(tmp, "T/S", col=2, add=TRUE)
                    }
                }
            }
        } else if (input$select == 2) {
            if (is.null(state$which)) {
                s <- length(d$time)
            } else {
                s <- state$which
            }
            imagep(d$icl[[s]][['time']], d$icl[[s]][['freq']], d$icl[[s]][['spec']],
                   ylab='Frequency [Hz]', drawTimeRange = FALSE,
                   zlab=d$time[s], col=oceColorsViridis,
                   zlim=c(0, 100), decimate=FALSE)
        } else if (input$select == 3) {
            if (is.null(state$whichips)) {
                s <- length(d$ipsTime)
            } else {
                s <- state$whichips
            }
            hist(d$ips[[s]]$range-d$pAtm[s], d$ips[[s]]$rangeBin-d$pAtm[s], main='',
                 xlab='Ice draft [m]')
            mtext(d$ipsTime[s], cex=1.75, line=1.4)
            box()
            abline(v=d$maxDraft[s] - d$pAtm[s], col=2, lwd=2)
            if (d$maxDraft[s]-d$pAtm[s] > 12) {
                mtext(paste0('Maximum Draft: ', format(d$maxDraft[s]-d$pAtm[s], digits=4), ' m'),
                      at=12,
                      cex=2, font=2, col=2, adj=1)
            } else if (d$maxDraft[s]-d$pAtm[s] < 2) {
                mtext(paste0('Maximum Draft: ', format(d$maxDraft[s]-d$pAtm[s], digits=4), ' m'),
                      at=d$maxDraft[s]-d$pAtm[s],
                      cex=2, font=2, col=2, adj=0)
            } else {
                mtext(paste0('Maximum Draft: ', format(d$maxDraft[s]-d$pAtm[s], digits=4), ' m'),
                      at=d$maxDraft[s]-d$pAtm[s],
                      cex=2, font=2, col=2, adj=1)
            }
        } else if (input$select == 4) {
            if (is.null(state$xlim)) {
                if (input$adp2 == "U") {
                    plot(d$enu, which=1)
                } else if (input$adp2 == "V") {
                    plot(d$enu, which=2)
                } else if (input$adp2 == "depth-averaged U") {
                    plot(d$enu, which=19)
                } else if (input$adp2 == "depth-averaged V") {
                    plot(d$enu, which=20)
                } else if (input$adp2 == "backscatter1") {
                    plot(d$enu, which=5)
                } else if (input$adp2 == "backscatter2") {
                    plot(d$enu, which=6)
                } else if (input$adp2 == "backscatter3") {
                    plot(d$enu, which=7)
                } else if (input$adp2 == "backscatter4") {
                    plot(d$enu, which=8)
                } else if (input$adp2 == "average backscatter") {
                    bsavg <- apply(beamUnspreadAdp(d$enu)[['a', 'numeric']], 1, mean, na.rm=TRUE)
                    oce.plot.ts(d$enu[['time']], bsavg, ylab='Average backscatter')
                } else {
                    plot(d$enu, which=input$adp2)
                }
            } else {
                if (input$adp2 == "U") {
                    plot(d$enu, which=1, xlim=state$xlim)
                } else if (input$adp2 == "V") {
                    plot(d$enu, which=2, xlim=state$xlim)
                } else if (input$adp2 == "depth-averaged U") {
                    plot(d$enu, which=19, xlim=state$xlim)
                } else if (input$adp2 == "depth-averaged V") {
                    plot(d$enu, which=20, xlim=state$xlim)
                } else if (input$adp2 == "backscatter1") {
                    plot(d$enu, which=5, xlim=state$xlim)
                } else if (input$adp2 == "backscatter2") {
                    plot(d$enu, which=6, xlim=state$xlim)
                } else if (input$adp2 == "backscatter3") {
                    plot(d$enu, which=7, xlim=state$xlim)
                } else if (input$adp2 == "backscatter4") {
                    plot(d$enu, which=8, xlim=state$xlim)
                } else if (input$adp2 == "average backscatter") {
                    bsavg <- apply(beamUnspreadAdp(d$enu)[['a', 'numeric']], 1, mean, na.rm=TRUE)
                    oce.plot.ts(d$enu[['time']], bsavg, ylab='Average backscatter', xlim=state$xlim)
                } else {
                    plot(d$enu, which=input$adp2, xlim=state$xlim)
                }
            }
        }
    }
    )
    
    observeEvent(input$plot_click, {
        if (input$select == 2) {
            state$which <- findNearest(d$time, input$plot_click$x)
        } else if (input$select == 3) {
            state$whichips <- findNearest(d$ipsTime, input$plot_click$x)
        }
    })
    observeEvent(input$plot_brush, {
        if (input$select == 1) {
            if (input$mc == "All") {
                state$brushed <- NULL
            } else {
                df <- data.frame(x=d$mc[[whichmc(input$mc)]][['time']], x=d$mc[[whichmc(input$mc)]][[input$field]])
                state$brushed <- brushedPoints(df, input$plot_brush, "x", "y", allRows=TRUE)$selected_
            }
        }## else {
        state$xlim <- c(input$plot_brush$xmin, input$plot_brush$xmax)
        #}
    })
    observeEvent(input$resetmc, {
        state$xlim <- range(d$time)
    })
    observeEvent(input$reseticl, {
        state$xlim <- range(d$time)
    })
    observeEvent(input$resetips, {
        state$xlim <- range(d$ipsTime)
    })
    observeEvent(input$resetadp, {
        state$xlim <- range(d$enu[['time']])
    })
    observeEvent(input$resetbaro, {
        state$xlim <- range(d$baro$time)
    })
    observeEvent(input$resetmet, {
        state$xlim <- range(d$met[['time']])
    })
})
