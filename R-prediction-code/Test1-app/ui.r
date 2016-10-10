library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Security Term Word Cloud"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      helpText("...WordCloud may take a few moments to load")
      
      
    ),
    #End of slider panel
    
    # Show a plot of the generated distribution
    mainPanel(
      #textOutput("text1")
      #plotOutput("distPlot")
      plotOutput("word.cloud1")
      
    )
    #End of main panel
  )
  #End of slider layout
)#End of fluid page
)#End of shiny
  