#ui backup
## app.R ##
library(shinydashboard)
library(shiny)
library(DT)
library(rpivotTable)
#Example - https://hernanresnizky.shinyapps.io/EAHU/

ui <- dashboardPage(
  dashboardHeader(title = "Datadelphia"),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Master Tracker Stats", tabName = "Master_Stat", icon = icon("navicon")),
      menuItem("Closed Tasks", tabName = "closed", icon = icon("dashboard")),
      menuItem("Opened Tasks", tabName = "opened", icon = icon("dashboard")),
      menuItem("Aggregate Open vs Closed", tabName = "timechart", icon = icon("dashboard")),
      menuItem("Open vs Closed Chart", tabName = "OpenVSClose", icon = icon("dashboard")),
      menuItem("Performance PivotTable", tabName = "pivot", icon = icon("dashboard")),
      menuSubItem(icon = NULL,
                  dateRangeInput("dates",
                                 "DateRange",
                                 #label = 'Date Range Input: yyyy-mm-dd',
                                 start = (Sys.Date() - 7), 
                                 end = Sys.Date()))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Master_Stat",
              fluidRow(
                div(style = 'overflow-x: scroll',
                    
                    box(DT::dataTableOutput('mydata1')) 
                    
                    
                    
                ))
      ),
      
      
      
      tabItem(tabName = "closed",
              fluidRow(
                
                valueBoxOutput("closed1",width = 4),
                valueBoxOutput("high_count",width = 4),
                valueBoxOutput("medium_count", width = 4),
                valueBoxOutput("low_count",width = 4),
                valueBoxOutput("info_count",width = 4)
              ),
              #h4("Cyber Intrusion Related Alerts Investigations and Outcomes"),
              
              
              
              
              
              #dateRangeInput("dates",
              #"Date Range",
              #start = (Sys.Date() - 7), 
              #end = Sys.Date()),
              fluidRow(
                box(
                  status = "warning", solidHeader = TRUE,
                  htmlOutput("piechart")
                )
              )
      ),
      tabItem(tabName = "opened",
              fluidRow(
                valueBoxOutput("opened",width = 4),
                box(
                  status = "warning", solidHeader = TRUE,
                  htmlOutput("piechart2")
                )
              )
      ),
      tabItem(tabName = "timechart",
              fluidRow(
                #valueBoxOutput("opened",width = 4),
                box(
                  status = "warning", solidHeader = TRUE,
                  htmlOutput("time")
                )
              )
      ),
      tabItem(tabName = "OpenVSClose",
              fluidRow(
                #valueBoxOutput("opened",width = 4),
                box(
                  status = "warning", solidHeader = TRUE,
                  htmlOutput("openVclose")
                )
              )
      ),
      tabItem(tabName = "pivot",
              fluidRow(
                #tags$head(tags$style(HTML('
                #.main-header .logo {
                #font-size: 20px;
                #}
                #'
                div(style = 'overflow-x: scroll',
                    #tags$p(sytle = "font-size: 50px;",
                    #valueBoxOutput("opened",width = 4),
                    #box(title = "Pivot", width=100, height=2000, stauts = "primary", solidHeader = TRUE,
                    #tags$head(tags$style( type = 'text/css', '#test {overflow-x: scroll;')),
                    
                    rpivotTableOutput("rpivot", height="100%", width="100%"))
                #)
              )
      )
      
      
    )
  )
)











