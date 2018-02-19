library(shiny)

shinyUI(fluidPage(titlePanel('BSRTO data browser'), 
                  sidebarLayout(sidebarPanel(
                      selectInput("select", label = h3("Select Data type"), 
                                  choices = list("Water properties" = 1,
                                                 "Sound" = 2,
                                                 "Ice thickness" = 3,
                                                 "Water currents" = 4,
                                                 "Shore Station barometer" = 5,
                                                 "Resolute weather data" = 6), selected = 1),
                      conditionalPanel(
                          condition = "input.select == 2",
                          actionButton("reseticl", "Reset axis")),
                      conditionalPanel(
                          condition = "input.select == 3",
                          actionButton("resetips", "Reset axis")),
                      conditionalPanel(
                          condition = "input.select == 4",
                          actionButton("resetadp", "Reset axis")),
                      conditionalPanel(
                          condition = "input.select == 5",
                          actionButton("resetbaro", "Reset axis")),
                      conditionalPanel(
                          condition = "input.select == 6",
                          actionButton("resetmet", "Reset axis")),
                      conditionalPanel(
                          condition = "input.select == 2 | input.select == 3",
                          h4("Click/Drag top panel to zoom. Double click to select a time")),
                      conditionalPanel(
                          condition = "input.select == 4 | input.select == 5 | input.select == 6",
                          h4("Click/Drag top panel to zoom.")),
                      conditionalPanel(
                          condition = "input.select == 1",
                          radioButtons("mc", "Instrument Depth", c("40m", "60m", "80m", "160m", "All"),
                                       selected = "40m")),
                      conditionalPanel(
                          condition = "input.select == 1",
                          radioButtons("field", "Parameter", c("temperature", "salinity", "pressure", "oxygen", "T/S"),
                                       selected="temperature")),
                      conditionalPanel(
                          condition = "input.select == 4",
                          radioButtons("adp", "Field (top)", c("depth-averaged U", "depth-averaged V", "U", "V", "backscatter1", "backscatter2", "backscatter3", "backscatter4", "average backscatter", "pitch", "roll", "bottomRange1", "bottomRange2", "bottomRange3", "bottomRange4", "bottomVelocity1", "bottomVelocity2", "bottomVelocity3", "bottomVelocity4"),
                                       selected = "depth-averaged U")),
                      conditionalPanel(
                          condition = "input.select == 4",
                          radioButtons("adp2", "Field (top)", c("depth-averaged U", "depth-averaged V", "U", "V", "backscatter1", "backscatter2", "backscatter3", "backscatter4", "average backscatter", "pitch", "roll", "bottomRange1", "bottomRange2", "bottomRange3", "bottomRange4", "bottomVelocity1", "bottomVelocity2", "bottomVelocity3", "bottomVelocity4"),
                                       selected = "depth-averaged V")),
                      conditionalPanel(
                          condition = "input.select == 5",
                          radioButtons("baro", "Field", c("pressure", "temperature"),
                                       selected = "pressure")),
                      conditionalPanel(
                          condition = "input.select == 6",
                          radioButtons("met", "Field", c("temperature", "pressure",
                                                         "u", "v", "stickPlot",
                                                         "speed", "direction",
                                                         "dewPoint", "humidity"),
                                       selected = "temperature"))),
                      mainPanel(fluidRow(plotOutput("plot", dblclick="plot_click",
                                                    brush = brushOpts(id="plot_brush",
                                                                      direction="x",
                                                                      resetOnNew = TRUE),
                                                    height="300px")),
                                fluidRow(plotOutput("plot2", height="300px")))))
        )
