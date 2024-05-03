library(shiny)
library(ggplot2)
library(tidyverse)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Veiklos kodas 692000"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Įmonė",
                     "Pasirinkite įmonę",choices = NULL)),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output,session) {
  data = readRDS('../data/692000.rds')
  updateSelectizeInput(session, "Įmonė", choices = data$name, server = T)
  output$plot = renderPlot (
    data%>%
      filter( name == input$Įmonė) %>%
      ggplot(aes(x = ym(month), y=numInsured)) + geom_line()+ geom_point() + theme_grey() + labs(x = "Mėnesis", y="Apdraustuju skaicius")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
