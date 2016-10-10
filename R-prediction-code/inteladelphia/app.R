## app.R ##
library(shinydashboard)
library(shiny)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Analyst Intelladelphia"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Master Signature List", tabName = "signature_list", icon = icon("navicon")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Rule Types", tabName = "rule_types", icon = icon("dedent")),      
      menuItem("Rule Type Not Fired", tabName = "no_fire", icon = icon("dedent")),
      menuItem("Asset Hosts", tabName = "asset_hosts", icon = icon("dedent")),
      menuItem("ADDM data", tabName = "addm_data", icon = icon("dashboard")),
      menuItem("ADDM Software", tabName = "addm_software", icon = icon("dedent")),
      menuItem("ADDM Software Packages", tabName = "addm_software_packages", icon = icon("dedent")),
      menuItem("Nexpose", tabName = "Nexpose_Scan_Data", icon = icon("dedent")),
      menuItem("Splunk Suricata Traffic", tabName = "Splunk_Suricata_90day", icon = icon("dedent")),
      menuItem("Splunk Dashboard", tabName = "splunkPlot", icon = icon("dashboard")),
      menuItem("Splunk Dashboard 2", tabName = "splunkPlot2", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "signature_list",
              fluidRow(
                DT::dataTableOutput('mydata1'))
              
      ),
      
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot_class")), #, width = 500, height =  700)),
                box( title = "ET PRO Bloomberg Rule Categories",
                     plotOutput("plot_class2")), #, width = 800, height =  700)),
                box(plotOutput("plot_cve")) #, width = 500, height =  700))
              )
              
      ),
      tabItem(tabName = "asset_hosts",
              fluidRow(
                DT::dataTableOutput('mydata3'))
      ),
      
      tabItem(tabName = "addm_data",
              fluidRow(
                box(plotOutput("plot_addm_host", width = 1000, height =  1000)))
      ),
      tabItem(tabName = "addm_software",
              fluidRow(
                DT::dataTableOutput('mydata2'))
      ),
      tabItem(tabName = "Nexpose_Scan_Data",
              fluidRow(
                DT::dataTableOutput('mydata7'))
      ),
      tabItem(tabName = "rule_types",
              fluidRow(
                DT::dataTableOutput('mydata5'))
      ),
      tabItem(tabName = "no_fire",
              fluidRow(
                DT::dataTableOutput('mydata8'))
      ),
      tabItem(tabName = "Splunk_Suricata_90day",
              fluidRow(
                DT::dataTableOutput('mydata6'))
              
      ),
      tabItem(tabName = "splunkPlot",
              fluidRow(
                box(plotOutput("splunk_plot", width = 2000, height =  1000)))
      ),
      tabItem(tabName = "splunkPlot2",
              fluidRow(
                box(plotOutput("splunk_plot2", width = 1000, height =  1000)),
                box(plotOutput("splunk_plot3", width = 1000, height =  1000)))
              
      ),
      
      tabItem(tabName = "addm_software_packages",
              fluidRow(
                DT::dataTableOutput('mydata4'))
      )
    )
  ),
)  

server <- function(input, output) {
  output$mydata1 = DT::renderDataTable(
    et.pro.modified.df_uq, options = list(paging = TRUE, lengthChange = TRUE, lengthMenu = c(10, 25, 50, 100))
  )
  output$plot_class <- renderPlot({
    ggplot(data = et.pro.df_classtype, aes(x = Var1, y = Freq, fill=Var1)) + geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("et.pro.df_classtype") +
      coord_flip()
  })
  output$mydata2 = DT::renderDataTable(
    ADDM_software, options = list(paging = TRUE, lengthChange = TRUE, lengthMenu = c(10, 25, 50, 100))
    
  )
  output$mydata7 = DT::renderDataTable(
    nexpose,  options = list(lengthChange = TRUE, searchable = TRUE,  lengthMenu = c(10, 25, 50, 100), autoWidth = TRUE)
    
  )
  output$mydata8 = DT::renderDataTable(
    no_match, options = list(lengthChange = TRUE, lengthMenu = c(10, 25, 50, 100))
    
  )
  
  output$mydata5 = DT::renderDataTable(
    Non_Bloomberg_SIGS, options = list(lengthChange = TRUE)
    
  )
  output$mydata6 = DT::renderDataTable(
    splunk, options = list(lengthChange = TRUE, lengthMenu = c(10, 25, 50, 100), autoWidth = TRUE)
    
  )
  
  
  output$mydata3 = DT::renderDataTable(
    ADDM_host, options = list(paging = TRUE, lengthChange = TRUE, lengthMenu = c(10, 25, 50, 100))
    
  )
  output$mydata4 = DT::renderDataTable(
    ADDM_software_packages, options = list(paging = TRUE, lengthChange = TRUE, lengthMenu = c(10, 25, 50, 100), autoWidth = TRUE)
  )
  
  output$plot_class2 <- renderPlot({
    ggplot(data = et.pro.modified_categories , aes(x = et.pro.modified_categories, y = Freq, fill=et.pro.modified_categories)) +  geom_bar(stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("ET Pro New Categories") +
      coord_flip()
  })
  output$plot_addm_host <- renderPlot({
    ggplot(data = ADDM_host_count , aes(x = reorder(Var1 , +Freq), y = Freq, fill= -Freq)) +  geom_bar(position = "dodge", stat="identity") + geom_text(aes(label=Freq), size=5) + ggtitle("ADDM Host Types") +
      coord_flip()
  })
  output$splunk_plot <- renderPlot({
    ggplot(splunk, aes(x = classtype , y = msg_type, label = sid )) +
      geom_point(aes(size = request_count, colour = percent_of_hits  , alpha=.80)) + 
      geom_text(hjust = 1.5, size = 3) +
      scale_size(range = c(1,15)) +
      theme_bw()
  })
  output$splunk_plot2 <- renderPlot({
    ggplot(data = splunk_msg_type , aes(x = reorder(splunk_msg , +Freq), y = Freq, fill= +Freq)) +   
      geom_bar(stat="identity") +  geom_text(aes(label=Freq, colour = factor(Var1)),  colour = "red", fontface = "bold", size=4.5) + 
      ggtitle("SPLUNK 90 Day Msg Type") + labs(x="Rule MSG Type",y="90 Day Alert Count") +   
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=.5)) +  
      theme(axis.text = element_text(size= 10)) +  
      coord_flip() 
    
  })
  output$splunk_plot3 <- renderPlot({
    ggplot(data = splunk_BLOOM_Category , aes(x = reorder(Var1 , +Freq), y = Freq, fill= +Freq)) +   
      geom_bar(stat="identity") +  geom_text(aes(label=Freq, colour = factor(Var1)),  colour = "red", fontface = "bold", size=4.5) + 
      ggtitle("SPLUNK 90 Day BLoomberg Category") + labs(x="Rule MSG Type",y="90 Day Alert Count") +   
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=.5)) +  
      theme(axis.text = element_text(size= 10)) +  
      coord_flip() 
    
  })
  
  output$plot_cve <- renderPlot({
    ggplot(data = CVE_year_et.pro, aes(x = Var1, y = Freq)) + ggtitle("ET PRO CVE Distribution per year") + geom_bar(stat="identity") + 
      geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)
  })
}


shinyApp(ui, server)

