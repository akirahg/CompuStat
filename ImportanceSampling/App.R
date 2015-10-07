library(shiny)
library(ggplot2)
source("ImportanceSampling.R")

ui <- fluidPage(
  titlePanel("Crude Monte Carlo & Importance Sampling"),
  sliderInput(inputId = "m",
              label = "Choose the parameter m",
              value = 1, min = 1, max = 100),
  mainPanel(
    plotOutput("Estim.Unif"),
    plotOutput("Estim.Exp"),
    plotOutput("Estim.Beta"),
    plotOutput("Errores1"),
    plotOutput("Errores2"),
    tableOutput("Full.Results")
  )
)

server <- function(input,output){
  output$Estim.Unif <- renderPlot({
    #Gráfica intervalos de confianza sim. uniforme
    ggplot(full.results<-FullDataGenerator(input$m),aes(x=Nsim)) +
      ggtitle("Crude Monte Carlo Estimation") +
      geom_ribbon(aes(ymin=LI.Unif, ymax=LU.Unif), fill="grey", alpha=.4) +
      geom_line(aes(y=Estim.Unif), colour="blue") +
      geom_hline(aes(yintercept=Real.Value), colour = "red", linetype="dotted", size=1)
  })
  output$Estim.Exp <- renderPlot({
    #Gráfica intervalos de confianza sim. exponencial
    ggplot(full.results<-FullDataGenerator(input$m),aes(x=Nsim)) +
      ggtitle("Truncated exponential estimation") +
      geom_ribbon(aes(ymin=LI.Exp, ymax=LU.Exp), fill="grey", alpha=.4) +
      geom_line(aes(y=Estim.Exp), colour="blue") +
      geom_hline(aes(yintercept=Real.Value), colour = "red", linetype="dotted", size=1)
  })
  output$Estim.Beta <- renderPlot({
    #Gráfica intervalos de confianza sim. beta
    ggplot(full.results<-FullDataGenerator(input$m),aes(x=Nsim)) +
      ggtitle("Beta(1,m*2) estimation") +
      geom_ribbon(aes(ymin=LI.Beta, ymax=LU.Beta), fill="grey", alpha=.4) +
      geom_line(aes(y=Estim.Beta), colour="blue") +
      geom_hline(aes(yintercept=Real.Value), colour = "red", linetype="dotted", size=1)
  })
  output$Errores1 <- renderPlot({
    #Gráfica de los errores
    ggplot(errores<-ErrorGenerator(input$m),aes(x=Nsim)) +
      ggtitle("Estimation errors") + 
      geom_line(aes(y=Error.Unif, colour = "Uniforme")) +
      geom_line(aes(y=Error.Beta, colour = "Beta"))
  })
  output$Errores2 <- renderPlot({
    ggplot(errores<-ErrorGenerator(input$m),aes(x=Nsim)) +
      ggtitle("Estimation errors for the truncated exponential") +
      geom_line(aes(y=Error.Exp, colour = "Exponencial"))
  })
  output$Full.Results <- renderTable(full.results<-FullDataGenerator(input$m))
}

shinyApp(ui = ui, server = server)

