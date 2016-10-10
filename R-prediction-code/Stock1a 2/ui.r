# ui.R file

library(shiny)

shinyUI(fluidPage(
  titlePanel("Assignment 6 Brennan Lodge"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("symb", label = "Choose a stock symbol",
                  choices = list("AAPL", "GOOG", "JPM", "GS", "BAC", "HACK", "SPLK"),
                  selected = "JPM"),
 
      br(),
      
      dateRangeInput("dates", 
                     "Date Range",
                     start = "2014-01-01", 
                     end = as.character(Sys.Date())),
      
      br(),
      
      selectInput(inputId = "prediction",
                  label = "Forecast Model",
                  choices = c(
                              "Nueral Network" = "nnetar",
                              "Moving average" = "ma",
                              "Random Walk" = "rwf"
                              ),
                  selected = "rwf",
      ),
      
      
      br()
      
    
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
))

