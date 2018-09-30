#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(NterminalRS)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- dashboardPagePlus(title = "N-terminal analysis of translation dynamics",
  dashboardHeaderPlus(),
  dashboardSidebar(
    textInput("pattern",
                "Input pattern here")
  ),
  dashboardBody(
    setShadow("box"),
    fluidRow(
      boxPlus(title = "Quick how-to", status = "info", width = 12,
              solidHeader = TRUE,
              HTML("Input aminoacid pattern on the left. It is searched from the beginning of the gene. It uses Perl-compatible
                    regular expressions. You can use for example [A-Z] to indicate any aminoacid or [A,I,L] to require one of those three
                   (alanine, isoleucine and leucine)."), collapsible = TRUE, closable = FALSE),
      boxPlus(
        title = "Main plot with dropdown",
        status = "warning",
        solidHeader = FALSE,
        closable = FALSE,
        collapsible = TRUE,
        width = 12,
        plotOutput("distPlot"),
        textOutput("dataSummary")
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  input.data <- readRDS(system.file("cached.data",
                          package = "NterminalRS"))


  data.for.plot <- reactive({
    message("pattern added")
    dataExtract(input.data, input$pattern)
  })

   output$distPlot <- renderPlot({
     message("Plot rendering")
     pattern_plot(data.for.plot(), isolate(input$pattern))
   })

   output$dataSummary <- renderText({
     summary.data <- dataSummary(data.for.plot())
     paste0("Number of valid observations: ", summary.data$observations, ", across ", summary.data$genes, " genes." )
   })
}

# Run the application
shinyApp(ui = ui, server = server)

