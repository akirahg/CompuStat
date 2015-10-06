library(shiny)
library(ggplot2)

ui <- fluidPage(
  sliderInput(inputId = "nsim",
              label = "Choose the sample size",
              value = 1000, min = 10, max = 10000),
  numericInput(inputId = "mean",
               label = "Indicate the mean of the normal distribution",
               value = 0),
  numericInput(inputId = "sd",
            label = "Indicate the standard deviation of the normal distribution",
            value = 1,
            min = 1, max = 100)
)

server <- function(input,output){
  output$hist <- renderPlot({
    ggplot(res,aes(x=Normal)) +
      geom_histogram(aes(x=BoxMuller1,y=..density..),binwidth=.5,colour="black",fill="white") +
      stat_function(fun = dnorm, args = list(mean=miu,sd=sigma), aes(colour = "Normal"), fill = "#FF6666")
  })
}

shinyApp(ui = ui, server = server)
