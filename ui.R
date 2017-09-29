library(shiny)

shinyUI(fluidPage(titlePanel('BSRTO data browser'), 
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
        )
