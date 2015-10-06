library(shiny)
library(ggplot2)
source("BoxMuller.R")

ui <- fluidPage(
  titlePanel("Box-Muller method simulation"),
  sliderInput(inputId = "nsim",
              label = "Choose the sample size",
              value = 1000, min = 10, max = 10000),
  numericInput(inputId = "mean",
               label = "Indicate the mean of the normal distribution",
               value = 0),
  numericInput(inputId = "sd",
            label = "Indicate the standard deviation of the normal distribution",
            value = 1,
            min = 1, max = 100),
  mainPanel(
    plotOutput("hist"),
    tableOutput("summary")
  )
)

server <- function(input,output){
  output$hist <- renderPlot({
    ggplot(res<-BoxMuller(input$nsim,input$mean,input$sd),aes(x=Normal)) +
      geom_histogram(aes(x=BoxMuller1,y=..density..),binwidth=.5,colour="black",fill="white") +
      stat_function(fun = dnorm, args = list(mean=input$mean,sd=input$sd), aes(colour = "Normal"), fill = "#FF6666")
  })
  output$summary <- renderTable(summary(res<-BoxMuller(input$nsim,input$mean,input$sd)))
}


shinyApp(ui = ui, server = server)
