# server.R file

library(quantmod)
library(forecast)
#discovered the forecast package found here 
#http://cran.r-project.org/web/packages/forecast/forecast.pdf


shinyServer(function(input, output) {
  #take the selection from the stock choice refrenceing the yahoo finance back-end
  output$plot <- renderPlot({
    data <- getSymbols(input$symb, src = "yahoo", 
                       from = input$dates[1],
                       to = input$dates[2],
                       auto.assign = FALSE)
    
    #set the date fields by converting the string to a date
    #Merging the date witthe stock to plot and index with the stock data
    data.date <- c(as.Date(index(data)), as.Date(tail(index(data),1)) + 0:4)
    
    data.adj = data[,6]
    
  
    #A switch statement to call the forecast models
    switch(input$prediction,
      "nnetar" = {
        data.chart <- nnetar(data.adj)
      },
      "rwf" = {
        data.chart <- rwf(data.adj)
      },
      "ma" = {
        data.chart <- ma(data.adj, order=5)
      }
    )
    
    #Assign the days to predict (5) ahead of todays date against the past dates
    daysToPredict <- 5
    data.forecast <- forecast(data.chart, h = daysToPredict)

    
    #Plot the days to predict with the input range
    plot(data.forecast, include=input$date.range, xaxt="n", yaxt="n")
    axis(1, at=index(data.date), labels=data.date)
    
  })
  
})
