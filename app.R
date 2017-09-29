library(shiny)
library(oce)
load('mc.rda')
load('ips.rda')
load('cl.rda')
ipsTime <- numberAsPOSIXct(unlist(lapply(ips, function(x) x[['time']])))
maxDraft <- unlist(lapply(ips, function(x) x[['maxDraft']]))
meanDraft <- unlist(lapply(ips, function(x) x[['meanDraft']]))
pAtm <- unlist(lapply(ips, function(x) x[['barometricPressure']])) - 25
timelist <- list()
for (i in seq_along(time)) {
    timelist[[i]] <- i
}
names(timelist) <- format(time)

mcplot <- function(x, field) {
    if (is.list(x)) {
        if (field == "T/S") {
            Srange <- range(unlist(lapply(x, function(x) x[['salinity']])), na.rm=TRUE)
            Trange <- range(unlist(lapply(x, function(x) x[['temperature']])), na.rm=TRUE)
            for (i in 1:length(x)) {
                if (i == 1) {
                    plotTS(x[[i]], cex=1.5, pch=21, pt.bg=i, Tlim=Trange, Slim=Srange)
                } else {
                    plotTS(x[[i]], cex=1.5, pch=21, pt.bg=i, add=TRUE)
                }
            }
        } else {
            range <- range(unlist(lapply(x, function(x) x[[field]])), na.rm=TRUE)
            for (i in 1:length(x)) {
                if (i == 1) {
                    oce.plot.ts(x[[i]][['time']], x[[i]][[field]], ylab=field, ylim=range)
                } else {
                    lines(x[[i]][['time']], x[[i]][[field]], col=i)
                }
            }
        }
    } else {
        if (field == "T/S") {
            plotTS(x, cex=1.5, pch=21, pt.bg='lightgrey')
        } else {
            if (field == 'oxygen' & !('oxygen' %in% names(x@data))) {
                plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
                box()
                text(0.5, 0.5, "No oxygen data available at this depth")
            } else {
                oce.plot.ts(x[['time']], x[[field]], ylab=field)
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

ui <- fluidPage(titlePanel('BSRTO data browser'), 
                sidebarLayout(sidebarPanel(
                    selectInput("select", label = h3("Select Data type"), 
                                choices = list("Water properties" = 1, "Sound" = 2, "Ice thickness" = 3), selected = 1),
                    conditionalPanel(
                        condition = "input.select == 1",
                        radioButtons("mc", "Instrument Depth", c("40m", "60m", "80m", "160m", "All"),
                                     selected = "40m")),
                    conditionalPanel(
                        condition = "input.select == 1",
                        radioButtons("field", "Parameter", c("temperature", "salinity", "pressure", "oxygen", "T/S"),
                                     selected="temperature")),
                    conditionalPanel(
                        condition = "input.select == 2 | input.select == 3",
                        h4("Click/Drag top panel to zoom. Double click to select a time")),
                    conditionalPanel(
                        condition = "input.select == 2",
                        actionButton("reseticl", "Reset plot")),
                    conditionalPanel(
                        condition = "input.select == 3",
                        actionButton("resetips", "Reset plot"))),
                    mainPanel(fluidRow(plotOutput("plot", dblclick="plot_click",
                                                  brush = brushOpts(id="plot_brush",
                                                                    direction="x",
                                                                    resetOnNew = TRUE),
                                                  height="300px")),
                              fluidRow(plotOutput("plot2", height="300px")))))

server <- function(input, output) {
    state <- reactiveValues()
    
    output$plot <- renderPlot({
        load('mc.rda')
        load('ips.rda')
        load('icl.rda')
        ipsTime <- numberAsPOSIXct(unlist(lapply(ips, function(x) x[['time']])))
        maxDraft <- unlist(lapply(ips, function(x) x[['maxDraft']]))
        meanDraft <- unlist(lapply(ips, function(x) x[['meanDraft']]))
        pAtm <- unlist(lapply(ips, function(x) x[['barometricPressure']])) - 25
        if (!is.null(input$select)) {
            if (input$select == 1) {
                if (input$mc == "40m") {
                    mcplot(mc[['mcI']], input$field)
                } else if (input$mc == "60m") {
                    mcplot(mc[['mcA']], input$field)
                } else if (input$mc == "80m") {
                    mcplot(mc[['imm']], input$field)
                } else if (input$mc == "160m") {
                    mcplot(mc[['mcH']], input$field)
                } else if (input$mc == "All") {
                    mcplot(mc, input$field)
                }
            } else if (input$select == 2) {
                if (is.null(state$xlim)) {
                    imagep(time, freq, spec, ylab='Frequency [Hz]', col=oceColorsViridis,
                           zlim=c(0, 40))
                } else {
                    imagep(time, freq, spec, ylab='Frequency [Hz]', col=oceColorsViridis,
                           zlim=c(0, 40), xlim=state$xlim)
                }
            } else if (input$select == 3) {
                if (is.null(state$xlim)) {
                    oce.plot.ts(ipsTime, maxDraft-pAtm/100, type='b', pch=3, ylim=c(-1, 11),
                                ylab='Ice draft [m]')
                    grid()
                    legend('topleft', c('Maximum Draft', 'Mean Draft'), pch=c(3, 1))
                    points(ipsTime, meanDraft-pAtm/100)
                } else {
                    oce.plot.ts(ipsTime, maxDraft-pAtm/100, type='b', pch=3, ylim=c(-1, 11),
                                ylab='Ice draft [m]', xlim=state$xlim)
                    grid()
                    legend('topleft', c('Maximum Draft', 'Mean Draft'), pch=c(3, 1))
                    points(ipsTime, meanDraft-pAtm/100)
                }
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, "Please choose a data type to display")
        }
    })

    output$plot2 <- renderPlot( {
        load('mc.rda')
        load('ips.rda')
        load('icl.rda')
        ipsTime <- numberAsPOSIXct(unlist(lapply(ips, function(x) x[['time']])))
        maxDraft <- unlist(lapply(ips, function(x) x[['maxDraft']]))
        meanDraft <- unlist(lapply(ips, function(x) x[['meanDraft']]))
        pAtm <- unlist(lapply(ips, function(x) x[['barometricPressure']])) - 25
        if (input$select == 1) {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")    
        } else if (input$select == 2) {
            if (is.null(state$which)) {
                s <- length(time)
            } else {
                s <- state$which
            }
            imagep(icl[[s]][['time']], icl[[s]][['freq']], icl[[s]][['spec']],
                   ylab='Frequency [Hz]', drawTimeRange = FALSE,
                   zlab=time[s], col=oceColorsViridis,
                   zlim=c(0, 40))
        } else if (input$select == 3) {
            if (is.null(state$whichips)) {
                s <- length(ipsTime)
            } else {
                s <- state$whichips
            }
            hist(ips[[s]]$range-pAtm[s]/100, ips[[s]]$rangeBin-pAtm[s]/100, main='',
                 xlab='Ice draft [m]')
            mtext(ipsTime[s], cex=1.75, line=1.4)
            box()
            abline(v=maxDraft[s] - pAtm/100)
            mtext(paste0(format(maxDraft[s]-pAtm[s]/100, digits=4), ' m'), at=maxDraft[s]-pAtm[s]/100,
                  cex=2, font=2, col=2)

        }
    }
    )
    
    observeEvent(input$plot_click, {
        if (input$select == 2) {
            state$which <- findNearest(time, input$plot_click$x)
        } else if (input$select == 3) {
            state$whichips <- findNearest(ipsTime, input$plot_click$x)
        }
    })
    observeEvent(input$plot_brush, {
        state$xlim <- c(input$plot_brush$xmin, input$plot_brush$xmax)
    })
    observeEvent(input$reseticl, {
        state$xlim <- range(time)
    })
    observeEvent(input$resetips, {
        state$xlim <- range(ipsTime)
    })
}

shinyApp(ui, server)
