#server backup
#metrics server
library(utils)
## Using Internet Explorer proxy settings is
## often helpful in an IT controlled environment
#.libPaths(c("C:\\Users\\blodge9\\Downloads", .libPaths()))

#using data tables
#https://rstudio.github.io/DT/

#include libraries 
library(googleVis)
library(jsonlite)
library(rvest)
library(RCurl)
library(httr)
library(XML)
library(RJSONIO)
library(stringr)
library(rpivotTable)
library(rjson)
library(stringr)
library(DT)
library(reshape2)
library(plyr)
library(R.utils)




server <- (function(input, output, session) {
  
  output$mydata1 = DT::renderDataTable({
    perf_grab <- getURL(paste0("http://10.124.139.237:5000/stats/analystTime?sdate=",input$dates[1],"&edate=",input$dates[2]))
    json_file <- fromJSON(perf_grab, method = "C", unexpected.escape = "error")
    
    
    json_file <- lapply(json_file, function(x) {
      x[sapply(x, is.null)] <- NA
      unlist(x)
    })
    
    #perf.df <- do.call("rbind", json_file)
    perf.df <- ldply(json_file, rbind)
    perf.df <- as.data.frame(perf.df)
    
    
    #split the description tags in []
    v <- str_split_fixed(perf.df$description, "]",3)
    v <- as.data.frame(v)
    v$V1 <- gsub('\\[',"",v$V1)
    v$V2 <- gsub('\\[',"",v$V2)
    v$V3 <- gsub('\\[',"",v$V3)
    colnames(v) <- c("severity", "bloomberg_tag", "log.source")
    v$log.source <- gsub('\\]',"",v$log.source)
    
    #combine the new tags
    perf.df <- cbind(perf.df,v)
    perf.df <- as.data.frame(perf.df)
    
    perf.df$owners <- as.character(perf.df$owners)
    
    
    #team assignments 
    perf.df$team[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|bsingh|kbrooks|erichardson',perf.df$owners, perl=TRUE, ignore.case = TRUE)] <- "Triage"
    perf.df$team[grepl('gdemitre|blodge|rcave|croose|wwelch|ggrisamore',perf.df$owners, perl=TRUE, ignore.case = TRUE)] <- "Analysis"
    perf.df$team[grepl('bhartstein|nsimonian|mnitulescu|miacovacci|bchang|sgopinath',perf.df$owners, perl=TRUE, ignore.case = TRUE)] <- "CIRT"
    perf.df$team[grepl('vjimenez',perf.df$owners, perl=TRUE, ignore.case = TRUE)] <- "Victors"
    
    
    perf.df$shift[grepl('bsingh|kbrooks|',perf.df$owners,perl=TRUE, ignore.case = TRUE)] <- "2"
    perf.df$shift[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|erichardson',perf.df$owners,perl=TRUE, ignore.case = TRUE)] <- "1"
    perf.df$shift[grepl('bhartstein|vjimenez|nsimonian|mnitulescu|miacovacci|bchang|sgopinath|gdemitre|blodge|rcave|croose|wwelch|ggrisamore|NA',perf.df$owners,perl=TRUE, ignore.case = TRUE)] <- "0"
    
    perf.df$shift <- as.numeric(perf.df$shift)
    
    #convert to date fields 
    perf.df$assigned_ts <- as.POSIXct(perf.df$assigned_ts)
    perf.df$trusted_escalated_ts <- as.POSIXct(perf.df$trusted_escalated_ts)
    perf.df$create_ts <- as.POSIXct(perf.df$create_ts)
    perf.df$escalated_ts <- as.POSIXct(perf.df$trusted_escalated_ts)
    perf.df$completed_ts <- as.POSIXct(perf.df$completed_ts)
    
    #Response Time
    perf.df$Response.Time <- as.numeric(round(difftime(perf.df$assigned_ts, perf.df$create_ts, units="hours"),2))
    
    #Resolution Time
    perf.df$Resolution.Time <- as.numeric(round(difftime(perf.df$completed_ts, perf.df$assigned_ts, units="hours"),2))
    
    #Completed
    perf.df$completed.t.f <- !is.na(perf.df$completed_ts)
    
    #completed within the last week
    today <- as.POSIXct(Sys.Date())
    week_ago <- as.POSIXct(Sys.Date() - 7)
    two.weeks.ago <- as.POSIXct(week_ago - 7)
    perf.df$completed.week <- perf.df$completed_ts>=week_ago
    
    #stale tasks are opened less than 2 weeks ago (with a completion of FALSE)
    perf.df$stale <- perf.df$create_ts<=two.weeks.ago
    
    #get week number completed
    perf.df$completed.week.num <- strftime(perf.df$completed_ts, "%U")
    
    #get month number opened 
    perf.df$open.month.num <- strftime(perf.df$create_ts, "%m")
    
    #escalation to resolution time
    perf.df$escalated.resolution.time <- as.numeric(round(difftime(perf.df$completed_ts, perf.df$escalated_ts, units = "hours"), 2))
    
    #acknolegement to escalation
    perf.df$ack.to.escalation <- as.numeric(round(difftime(perf.df$assigned_ts, perf.df$escalated_ts, units = "hours"), 2))
    
    
    
    datatable(perf.df, filter = 'top',options = list(lengthChange = TRUE, searchable = TRUE,  lengthMenu = c(10, 20, 60, 100), autoWidth = TRUE))
  })
  datasetInput.closed <- reactive({
    ## Doughnut chart - a pie with a hole
    conclusion_site <- getURL(paste0("http://10.124.139.237:5000/stats/query1?sdate=",input$dates[1],"&edate=", input$dates[2]))
    conclusion_tags <- fromJSON(conclusion_site, method = "C", unexpected.escape = "error")
    conclusion_tags <- as.data.frame.AsIs(conclusion_tags, stringsAsFactors = FALSE)
    conclusion_tags <- cbind(Row.Names = rownames(conclusion_tags), conclusion_tags)
    
    s <- str_split_fixed(conclusion_tags$Row.Names, ":",3)
    c_s <- cbind(s,conclusion_tags$conclusion_tags)
    c_s <- as.data.frame(c_s)
    colnames(c_s) <- c("analyst", "type_tag", "conclusion", "count" )
    c_s$type_tag <- as.character(c_s$type_tag)
    v <- str_split_fixed(c_s$type_tag, "]",3)
    v <- as.data.frame(v)
    v$V1 <- gsub('\\[',"",v$V1)
    v$V2 <- gsub('\\[',"",v$V2)
    v$V3 <- gsub('\\[',"",v$V3)
    colnames(v) <- c("severity", "bloomberg_tag", "source1")
    v$source1 <- gsub('\\]',"",v$source1)
    
    c_v_s <- cbind(c_s,v)
    c_v_s$bloomberg_tag <- str_trim(c_v_s$bloomberg_tag)
    #c_v_s$analyst <- str_trim(c_v_s$analyst)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    c_v_s$bloomberg_tag <- gsub("Conclusion\\_\\_","",c_v_s$bloomberg_tag)
    c_v_s$conclusion <- gsub("Conclusion\\_\\_","",c_v_s$conclusion)
    c_v_s$source1 <- str_trim(c_v_s$source1)
    c_v_s$count <- as.numeric(c_v_s$count)
    c_v_s$count <- str_trim(c_v_s$count)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    
    c_v_s$team[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|bsingh|kbrooks',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Triage"
    c_v_s$team[grepl('gdemitre|blodge|rcave|croose|wwelch',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Analysis"
    c_v_s$team[grepl('bhartstein|nsimonian|mnitulescu|NA|miacovacci|bchang|sgopinath',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "CIRT"
    c_v_s$team[grepl('vjimenez',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Victors"
    
    
    c_v_s$conclusion <- sub("^$", "No threat legitimate", c_v_s$conclusion)
    c_v_s$conclusion <- sub("Nil", "No threat legitimate", c_v_s$conclusion)
    
    
    c_v_s$count <- as.numeric(c_v_s$count)
    clousure.count <- as.data.frame(aggregate(count ~ conclusion, c_v_s, sum))
    
    dat1 <- clousure.count
    dat1 <- dat1 [order(dat1$count),]
    
    dat1$merge <- paste(dat1$conclusion, dat1$count, sep=" - ")
    dat1 <- cbind(dat1$merge, dat1$count)
    dat1 <- as.data.frame(dat1)
    colnames(dat1) <- c("conclusion", "count")
    dat1$count <- str_trim(dat1$count)
    dat1$conclusion <- str_trim(dat1$conclusion)
    dat1$conclusion <- as.character(dat1$conclusion)
    dat1$count <- as.numeric(dat1$count)
    
    closed <- sum(dat1$count)
    gvisPieChart(dat1, 
                 options=list(
                   width=900,
                   height=600,
                   title='Weekly Cyber Intrusion Related Alerts Closed',
                   colors="['#F44336','#F44336','#FFA000','#FFA000','#4DB6AC','#109618','#AAAA11', '#0072B2','#009E73', '#009E73', '#009E73']",
                   #pieSliceText='percentage',
                   legend="right",
                   sliceVisibilityThreshold=0,
                   #is3D=TRUE,
                   #text(0,0,labels="Total", cex=1.5, font=2),
                   pieHole=0.5),
                 chartid="doughnut")                                                      
    
  })
  output$piechart <- renderGvis({datasetInput.closed()})
  
  datasetInput.opened <- reactive({
    open_tags_site <- getURL(paste0("http://10.124.139.237:5000/stats/kcount?sdate=",input$dates[1] ,"&edate=", input$dates[2]))
    created_tags <- fromJSON(open_tags_site, method = "C", unexpected.escape = "error")
    created_tags <- as.data.frame(created_tags)
    created_tags <- t(created_tags[,2:ncol(created_tags)])
    colnames(created_tags) <- created_tags[1,]
    created_tags <- cbind(tags = rownames(created_tags), created_tags)
    
    
    created_tags <- as.data.frame(created_tags)
    colnames(created_tags)  <- c("tag", "count")
    rownames(created_tags) <- NULL
    names(created_tags)
    created.tag.type <- subset(created_tags, tag %in% c("activity.review","malware","policy","phishing","browser.exploit",
                                                        "systems.health","spyware.or.adware.or.pup","dlp"))
    created.tag.type <- as.data.frame(created.tag.type)
    
    
    rownames(created.tag.type) <- NULL
    created.tag.type$count <- str_trim(created.tag.type$count)
    created.tag.type$count <- as.numeric(created.tag.type$count)
    created.tag.type  <- created.tag.type [order(-created.tag.type$count),]
    created.tag.type$merge <- paste(created.tag.type$tag, created.tag.type$count, sep=" - ")
    
    dat2 <- cbind(created.tag.type$merge, created.tag.type$count)
    dat2 <- as.data.frame(dat2)
    colnames(dat2) <- c("tag", "count")
    dat2$count <- str_trim(dat2$count)
    dat2$count <- as.numeric(dat2$count)
    opened <- sum(dat2$count)
    opened
    
    
    
    ## Doughnut chart - a pie with a hole
    gvisPieChart(dat2,
                 options=list(
                   width=950,
                   height=500,
                   title='Weekly Cyber Intrusion Related Alerts Opened',
                   colors="['#F44336','#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7' ]",
                   #pieSliceText='percentage',
                   legend="right",
                   sliceVisibilityThreshold=0,
                   pieHole=0.4),
                 chartid="doughnut")                                                      
  })
  output$piechart2 <- renderGvis({datasetInput.opened()})
  output$opened <- renderValueBox({
    open_tags_site <- getURL(paste0("http://10.124.139.237:5000/stats/kcount?sdate=",input$dates[1] ,"&edate=", input$dates[2]))
    created_tags <- fromJSON(open_tags_site, method = "C", unexpected.escape = "error")
    created_tags <- as.data.frame(created_tags)
    created_tags <- t(created_tags[,2:ncol(created_tags)])
    colnames(created_tags) <- created_tags[1,]
    created_tags <- cbind(tags = rownames(created_tags), created_tags)
    
    
    created_tags <- as.data.frame(created_tags)
    colnames(created_tags)  <- c("tag", "count")
    rownames(created_tags) <- NULL
    names(created_tags)
    created.tag.type <- subset(created_tags, tag %in% c("activity.review","malware","policy","phishing","browser.exploit",
                                                        "systems.health","spyware.or.adware.or.pup","dlp"))
    created.tag.type <- as.data.frame(created.tag.type)
    
    
    rownames(created.tag.type) <- NULL
    created.tag.type$count <- str_trim(created.tag.type$count)
    created.tag.type$count <- as.numeric(created.tag.type$count)
    created.tag.type  <- created.tag.type [order(-created.tag.type$count),]
    created.tag.type$merge <- paste(created.tag.type$tag, created.tag.type$count, sep=" - ")
    
    dat2 <- cbind(created.tag.type$merge, created.tag.type$count)
    dat2 <- as.data.frame(dat2)
    colnames(dat2) <- c("tag", "count")
    dat2$count <- str_trim(dat2$count)
    dat2$count <- as.numeric(dat2$count)
    opened <- sum(dat2$count)
    
    valueBox(
      value = opened,
      subtitle = "Number of Opened Tasks",
      icon = icon("users")
    )
    
  })
  
  
  output$closed1 <- renderValueBox({
    ## Doughnut chart - a pie with a hole
    conclusion_site <- getURL(paste0("http://10.124.139.237:5000/stats/query1?sdate=",input$dates[1],"&edate=", input$dates[2]))
    conclusion_tags <- fromJSON(conclusion_site, method = "C", unexpected.escape = "error")
    conclusion_tags <- as.data.frame.AsIs(conclusion_tags, stringsAsFactors = FALSE)
    conclusion_tags <- cbind(Row.Names = rownames(conclusion_tags), conclusion_tags)
    
    s <- str_split_fixed(conclusion_tags$Row.Names, ":",3)
    c_s <- cbind(s,conclusion_tags$conclusion_tags)
    c_s <- as.data.frame(c_s)
    colnames(c_s) <- c("analyst", "type_tag", "conclusion", "count" )
    c_s$type_tag <- as.character(c_s$type_tag)
    v <- str_split_fixed(c_s$type_tag, "]",3)
    v <- as.data.frame(v)
    v$V1 <- gsub('\\[',"",v$V1)
    v$V2 <- gsub('\\[',"",v$V2)
    v$V3 <- gsub('\\[',"",v$V3)
    colnames(v) <- c("severity", "bloomberg_tag", "source1")
    v$source1 <- gsub('\\]',"",v$source1)
    
    c_v_s <- cbind(c_s,v)
    c_v_s$bloomberg_tag <- str_trim(c_v_s$bloomberg_tag)
    #c_v_s$analyst <- str_trim(c_v_s$analyst)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    c_v_s$bloomberg_tag <- gsub("Conclusion\\_\\_","",c_v_s$bloomberg_tag)
    c_v_s$conclusion <- gsub("Conclusion\\_\\_","",c_v_s$conclusion)
    c_v_s$source1 <- str_trim(c_v_s$source1)
    c_v_s$count <- as.numeric(c_v_s$count)
    c_v_s$count <- str_trim(c_v_s$count)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    
    c_v_s$team[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|bsingh|kbrooks',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Triage"
    c_v_s$team[grepl('gdemitre|blodge|rcave|croose|wwelch',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Analysis"
    c_v_s$team[grepl('bhartstein|nsimonian|mnitulescu|NA|miacovacci|bchang|sgopinath',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "CIRT"
    c_v_s$team[grepl('vjimenez',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Victors"
    
    c_v_s$count <- as.numeric(c_v_s$count)
    clousure.count <- as.data.frame(aggregate(count ~ conclusion, c_v_s, sum))
    
    dat1 <- clousure.count
    dat1 <- dat1 [order(dat1$count),]
    dat1[dat1==""] <- NA
    dat1[dat1=="Nil"] <- NA
    #dat1 <- na.omit(dat1)
    
    dat1$merge <- paste(dat1$conclusion, dat1$count, sep=" - ")
    dat1 <- cbind(dat1$merge, dat1$count)
    dat1 <- as.data.frame(dat1)
    colnames(dat1) <- c("conclusion", "count")
    dat1$count <- str_trim(dat1$count)
    dat1$conclusion <- str_trim(dat1$conclusion)
    dat1$conclusion <- as.character(dat1$conclusion)
    dat1$count <- as.numeric(dat1$count)
    
    closed <- sum(dat1$count)
    
    valueBox(
      value = closed,
      subtitle = "Number of Closed Tasks",
      icon = icon("users")
    )
    
  })
  output$high_count <- renderValueBox({
    ## Doughnut chart - a pie with a hole
    conclusion_site <- getURL(paste0("http://10.124.139.237:5000/stats/query1?sdate=",input$dates[1],"&edate=", input$dates[2]))
    conclusion_tags <- fromJSON(conclusion_site, method = "C", unexpected.escape = "error")
    conclusion_tags <- as.data.frame.AsIs(conclusion_tags, stringsAsFactors = FALSE)
    conclusion_tags <- cbind(Row.Names = rownames(conclusion_tags), conclusion_tags)
    
    s <- str_split_fixed(conclusion_tags$Row.Names, ":",3)
    c_s <- cbind(s,conclusion_tags$conclusion_tags)
    c_s <- as.data.frame(c_s)
    colnames(c_s) <- c("analyst", "type_tag", "conclusion", "count" )
    c_s$type_tag <- as.character(c_s$type_tag)
    v <- str_split_fixed(c_s$type_tag, "]",3)
    v <- as.data.frame(v)
    v$V1 <- gsub('\\[',"",v$V1)
    v$V2 <- gsub('\\[',"",v$V2)
    v$V3 <- gsub('\\[',"",v$V3)
    colnames(v) <- c("severity", "bloomberg_tag", "source1")
    v$source1 <- gsub('\\]',"",v$source1)
    
    c_v_s <- cbind(c_s,v)
    c_v_s$bloomberg_tag <- str_trim(c_v_s$bloomberg_tag)
    #c_v_s$analyst <- str_trim(c_v_s$analyst)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    c_v_s$bloomberg_tag <- gsub("Conclusion\\_\\_","",c_v_s$bloomberg_tag)
    c_v_s$conclusion <- gsub("Conclusion\\_\\_","",c_v_s$conclusion)
    c_v_s$source1 <- str_trim(c_v_s$source1)
    c_v_s$count <- as.numeric(c_v_s$count)
    c_v_s$count <- str_trim(c_v_s$count)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    
    c_v_s$team[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|bsingh|kbrooks',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Triage"
    c_v_s$team[grepl('gdemitre|blodge|rcave|croose|wwelch',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Analysis"
    c_v_s$team[grepl('bhartstein|nsimonian|mnitulescu|NA|miacovacci|bchang|sgopinath',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "CIRT"
    c_v_s$team[grepl('vjimenez',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Victors"
    
    c_v_s$count <- as.numeric(c_v_s$count)
    clousure.count <- as.data.frame(aggregate(count ~ conclusion, c_v_s, sum))
    
    dat1 <- clousure.count
    dat1 <- dat1 [order(dat1$count),]
    dat1[dat1==""] <- NA
    dat1[dat1=="Nil"] <- NA
    #dat1 <- na.omit(dat1)
    
    dat1$merge <- paste(dat1$conclusion, dat1$count, sep=" - ")
    dat1 <- cbind(dat1$merge, dat1$count)
    dat1 <- as.data.frame(dat1)
    colnames(dat1) <- c("conclusion", "count")
    dat1$count <- str_trim(dat1$count)
    dat1$conclusion <- str_trim(dat1$conclusion)
    dat1$conclusion <- as.character(dat1$conclusion)
    dat1$count <- as.numeric(dat1$count)
    
    closed <- sum(dat1$count)
    severity <- as.data.frame(aggregate(count ~ severity, data = c_v_s, FUN = "sum"))
    high_count <- subset(severity, severity == "high", select = count)
    
    
    valueBox(
      value = high_count,
      subtitle = "Number High Closed Tasks",
      icon = icon("users")
    )
  })
  
  output$medium_count <- renderValueBox({
    ## Doughnut chart - a pie with a hole
    conclusion_site <- getURL(paste0("http://10.124.139.237:5000/stats/query1?sdate=",input$dates[1],"&edate=", input$dates[2]))
    conclusion_tags <- fromJSON(conclusion_site, method = "C", unexpected.escape = "error")
    conclusion_tags <- as.data.frame.AsIs(conclusion_tags, stringsAsFactors = FALSE)
    conclusion_tags <- cbind(Row.Names = rownames(conclusion_tags), conclusion_tags)
    
    s <- str_split_fixed(conclusion_tags$Row.Names, ":",3)
    c_s <- cbind(s,conclusion_tags$conclusion_tags)
    c_s <- as.data.frame(c_s)
    colnames(c_s) <- c("analyst", "type_tag", "conclusion", "count" )
    c_s$type_tag <- as.character(c_s$type_tag)
    v <- str_split_fixed(c_s$type_tag, "]",3)
    v <- as.data.frame(v)
    v$V1 <- gsub('\\[',"",v$V1)
    v$V2 <- gsub('\\[',"",v$V2)
    v$V3 <- gsub('\\[',"",v$V3)
    colnames(v) <- c("severity", "bloomberg_tag", "source1")
    v$source1 <- gsub('\\]',"",v$source1)
    
    c_v_s <- cbind(c_s,v)
    c_v_s$bloomberg_tag <- str_trim(c_v_s$bloomberg_tag)
    #c_v_s$analyst <- str_trim(c_v_s$analyst)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    c_v_s$bloomberg_tag <- gsub("Conclusion\\_\\_","",c_v_s$bloomberg_tag)
    c_v_s$conclusion <- gsub("Conclusion\\_\\_","",c_v_s$conclusion)
    c_v_s$source1 <- str_trim(c_v_s$source1)
    c_v_s$count <- as.numeric(c_v_s$count)
    c_v_s$count <- str_trim(c_v_s$count)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    
    c_v_s$team[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|bsingh|kbrooks',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Triage"
    c_v_s$team[grepl('gdemitre|blodge|rcave|croose|wwelch',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Analysis"
    c_v_s$team[grepl('bhartstein|nsimonian|mnitulescu|NA|miacovacci|bchang|sgopinath',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "CIRT"
    c_v_s$team[grepl('vjimenez',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Victors"
    
    c_v_s$count <- as.numeric(c_v_s$count)
    clousure.count <- as.data.frame(aggregate(count ~ conclusion, c_v_s, sum))
    
    dat1 <- clousure.count
    dat1 <- dat1 [order(dat1$count),]
    dat1[dat1==""] <- NA
    dat1[dat1=="Nil"] <- NA
    #dat1 <- na.omit(dat1)
    
    dat1$merge <- paste(dat1$conclusion, dat1$count, sep=" - ")
    dat1 <- cbind(dat1$merge, dat1$count)
    dat1 <- as.data.frame(dat1)
    colnames(dat1) <- c("conclusion", "count")
    dat1$count <- str_trim(dat1$count)
    dat1$conclusion <- str_trim(dat1$conclusion)
    dat1$conclusion <- as.character(dat1$conclusion)
    dat1$count <- as.numeric(dat1$count)
    
    closed <- sum(dat1$count)
    severity <- as.data.frame(aggregate(count ~ severity, data = c_v_s, FUN = "sum"))
    medium <- subset(severity, severity == "medium" | severity == "med", select = count)
    medium <- sum(medium)
    
    valueBox(
      value = medium,
      subtitle = "Number of Medium Tasks",
      icon = icon("users")
    )
  })
  output$low_count <- renderValueBox({
    ## Doughnut chart - a pie with a hole
    conclusion_site <- getURL(paste0("http://10.124.139.237:5000/stats/query1?sdate=",input$dates[1],"&edate=", input$dates[2]))
    conclusion_tags <- fromJSON(conclusion_site, method = "C", unexpected.escape = "error")
    conclusion_tags <- as.data.frame.AsIs(conclusion_tags, stringsAsFactors = FALSE)
    conclusion_tags <- cbind(Row.Names = rownames(conclusion_tags), conclusion_tags)
    
    s <- str_split_fixed(conclusion_tags$Row.Names, ":",3)
    c_s <- cbind(s,conclusion_tags$conclusion_tags)
    c_s <- as.data.frame(c_s)
    colnames(c_s) <- c("analyst", "type_tag", "conclusion", "count" )
    c_s$type_tag <- as.character(c_s$type_tag)
    v <- str_split_fixed(c_s$type_tag, "]",3)
    v <- as.data.frame(v)
    v$V1 <- gsub('\\[',"",v$V1)
    v$V2 <- gsub('\\[',"",v$V2)
    v$V3 <- gsub('\\[',"",v$V3)
    colnames(v) <- c("severity", "bloomberg_tag", "source1")
    v$source1 <- gsub('\\]',"",v$source1)
    
    c_v_s <- cbind(c_s,v)
    c_v_s$bloomberg_tag <- str_trim(c_v_s$bloomberg_tag)
    #c_v_s$analyst <- str_trim(c_v_s$analyst)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    c_v_s$bloomberg_tag <- gsub("Conclusion\\_\\_","",c_v_s$bloomberg_tag)
    c_v_s$conclusion <- gsub("Conclusion\\_\\_","",c_v_s$conclusion)
    c_v_s$source1 <- str_trim(c_v_s$source1)
    c_v_s$count <- as.numeric(c_v_s$count)
    c_v_s$count <- str_trim(c_v_s$count)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    
    c_v_s$team[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|bsingh|kbrooks',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Triage"
    c_v_s$team[grepl('gdemitre|blodge|rcave|croose|wwelch',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Analysis"
    c_v_s$team[grepl('bhartstein|nsimonian|mnitulescu|NA|miacovacci|bchang|sgopinath',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "CIRT"
    c_v_s$team[grepl('vjimenez',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Victors"
    
    c_v_s$count <- as.numeric(c_v_s$count)
    clousure.count <- as.data.frame(aggregate(count ~ conclusion, c_v_s, sum))
    
    dat1 <- clousure.count
    dat1 <- dat1 [order(dat1$count),]
    dat1[dat1==""] <- NA
    dat1[dat1=="Nil"] <- NA
    #dat1 <- na.omit(dat1)
    
    dat1$merge <- paste(dat1$conclusion, dat1$count, sep=" - ")
    dat1 <- cbind(dat1$merge, dat1$count)
    dat1 <- as.data.frame(dat1)
    colnames(dat1) <- c("conclusion", "count")
    dat1$count <- str_trim(dat1$count)
    dat1$conclusion <- str_trim(dat1$conclusion)
    dat1$conclusion <- as.character(dat1$conclusion)
    dat1$count <- as.numeric(dat1$count)
    
    closed <- sum(dat1$count)
    severity <- as.data.frame(aggregate(count ~ severity, data = c_v_s, FUN = "sum"))
    low <- subset(severity, severity == "low", select = count)
    
    valueBox(
      value = low,
      subtitle = "Number Low Medium Tasks",
      icon = icon("users")
    )
  })
  output$info_count <- renderValueBox({
    ## Doughnut chart - a pie with a hole
    conclusion_site <- getURL(paste0("http://10.124.139.237:5000/stats/query1?sdate=",input$dates[1],"&edate=", input$dates[2]))
    conclusion_tags <- fromJSON(conclusion_site, method = "C", unexpected.escape = "error")
    conclusion_tags <- as.data.frame.AsIs(conclusion_tags, stringsAsFactors = FALSE)
    conclusion_tags <- cbind(Row.Names = rownames(conclusion_tags), conclusion_tags)
    
    s <- str_split_fixed(conclusion_tags$Row.Names, ":",3)
    c_s <- cbind(s,conclusion_tags$conclusion_tags)
    c_s <- as.data.frame(c_s)
    colnames(c_s) <- c("analyst", "type_tag", "conclusion", "count" )
    c_s$type_tag <- as.character(c_s$type_tag)
    v <- str_split_fixed(c_s$type_tag, "]",3)
    v <- as.data.frame(v)
    v$V1 <- gsub('\\[',"",v$V1)
    v$V2 <- gsub('\\[',"",v$V2)
    v$V3 <- gsub('\\[',"",v$V3)
    colnames(v) <- c("severity", "bloomberg_tag", "source1")
    v$source1 <- gsub('\\]',"",v$source1)
    
    c_v_s <- cbind(c_s,v)
    c_v_s$bloomberg_tag <- str_trim(c_v_s$bloomberg_tag)
    #c_v_s$analyst <- str_trim(c_v_s$analyst)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    c_v_s$bloomberg_tag <- gsub("Conclusion\\_\\_","",c_v_s$bloomberg_tag)
    c_v_s$conclusion <- gsub("Conclusion\\_\\_","",c_v_s$conclusion)
    c_v_s$source1 <- str_trim(c_v_s$source1)
    c_v_s$count <- as.numeric(c_v_s$count)
    c_v_s$count <- str_trim(c_v_s$count)
    #c_v_s$analyst <- as.character(c_v_s$analyst)
    
    c_v_s$team[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|bsingh|kbrooks',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Triage"
    c_v_s$team[grepl('gdemitre|blodge|rcave|croose|wwelch',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Analysis"
    c_v_s$team[grepl('bhartstein|nsimonian|mnitulescu|NA|miacovacci|bchang|sgopinath',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "CIRT"
    c_v_s$team[grepl('vjimenez',c_v_s$analyst, perl=TRUE, ignore.case = TRUE)] <- "Victors"
    
    c_v_s$count <- as.numeric(c_v_s$count)
    clousure.count <- as.data.frame(aggregate(count ~ conclusion, c_v_s, sum))
    
    dat1 <- clousure.count
    dat1 <- dat1 [order(dat1$count),]
    dat1[dat1==""] <- NA
    dat1[dat1=="Nil"] <- NA
    #dat1 <- na.omit(dat1)
    
    dat1$merge <- paste(dat1$conclusion, dat1$count, sep=" - ")
    dat1 <- cbind(dat1$merge, dat1$count)
    dat1 <- as.data.frame(dat1)
    colnames(dat1) <- c("conclusion", "count")
    dat1$count <- str_trim(dat1$count)
    dat1$conclusion <- str_trim(dat1$conclusion)
    dat1$conclusion <- as.character(dat1$conclusion)
    dat1$count <- as.numeric(dat1$count)
    
    closed <- sum(dat1$count)
    severity <- as.data.frame(aggregate(count ~ severity, data = c_v_s, FUN = "sum"))
    info <- subset(severity, severity == "info", select = count)
    
    valueBox(
      value = info,
      subtitle = "Number of Info Tasks",
      icon = icon("users")
    )
  })
  datasetInput.aggregate <- reactive({
    #backlog rework to count open and closed tasks 
    #backlog rework to count open and closed tasks 
    current_date <- Sys.Date()
    week_ago <- Sys.Date() - 7 
    current.week.number <- strftime(current_date,format="%W")
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    year_2016 <- as.Date("2016-01-01")
    
    backlog <- getURL(paste0("http://10.124.139.237:5000/stats/query2?sdate=",input$dates[1],"&edate=", input$dates[2]))
    backlog <- fromJSON(backlog, method = "C", unexpected.escape = "error")
    backlog <- as.data.frame(backlog)
    
    
    backlog.split <- str_split_fixed(backlog$backlog, ",",3)
    backlog.split <- as.data.frame(backlog.split)
    colnames(backlog.split) <- c("tags", "open.date", "close.date")
    backlog.split2 <- str_split_fixed(backlog.split$tags, "]",3)
    backlog.split2 <- as.data.frame(backlog.split2)
    backlog.split2$V1 <- gsub('\\[',"",backlog.split2$V1)
    backlog.split2$V2 <- gsub('\\[',"",backlog.split2$V2)
    backlog.split2$V3 <- gsub('\\[',"",backlog.split2$V3)
    backlog.split2$V3 <- gsub('\\]',"",backlog.split2$V3)
    colnames(backlog.split2) <- c("severity", "bloomberg_tag", "rule")
    
    backlog.master <- cbind(backlog.split,backlog.split2)
    backlog.master$bloomberg_tag <- str_trim(backlog.master$bloomberg_tag)
    backlog.master$src <- trim(backlog.master$rule)
    backlog.master$src <- as.character(backlog.master$rule)
    backlog.master$open.date <- as.POSIXct(strptime(backlog.master$open.date, "%Y-%m-%d"))
    backlog.master$close.date <- as.POSIXct(strptime(backlog.master$close.date, "%Y-%m-%d"))
    
    
    
    #split 
    backlog.master$rule_extract <- gsub('[^ ]*$',"",backlog.master$rule)
    backlog.master$rule_extract <- gsub('\\:',"",backlog.master$rule_extract)
    backlog.master$src <- str_extract(backlog.master$rule_extract, '\\w+')
    
    
    
    backlog.master$close.week.number <- strftime(backlog.master$close.date,format="%W")
    backlog.master$close.month.number <- strftime(backlog.master$close.date,format="%m")
    backlog.master$close.month.name <- strftime(backlog.master$close.date,format="%B")
    
    
    
    backlog.master$open.week.number <- strftime(backlog.master$open.date,format="%W")
    backlog.master$open.month.number <- strftime(backlog.master$open.date,format="%m")
    backlog.master$open.month.name <- strftime(backlog.master$open.date,format="%B")
    
    open.day <- as.data.frame(table(backlog.master$open.date))
    open.day$type <- "open"
    close.day <- as.data.frame(table(backlog.master$close.date))
    close.day$type <- "close"
    
    open_v_close <- rbind(open.day, close.day)
    open_v_close <- as.data.frame(open_v_close)
    open_v_close$Var1 <- as.Date(open_v_close$Var1)
    open_v_close$Freq <- as.numeric(open_v_close$Freq)
    open_v_close$type <- as.character(open_v_close$type)
    open_v_close  <- open_v_close[order(as.Date(open_v_close$Var1, format="%Y/%m/%d")),]
    
    
    
    
    close1 <- subset(open_v_close, type == "close")
    close1 <- as.data.frame(close1)
    open1 <- subset(open_v_close, type == "open")
    open1 <- as.data.frame(open1)
    open1$cumsum <- cumsum(open1$Freq)
    close1$cumsum <- cumsum(close1$Freq)
    
    close1$Var1 <- as.Date(close1$Var1)
    close1$Freq <- as.numeric(close1$Freq)
    close1$type <- as.character(close1$type)
    
    open1$Var1 <- as.Date(open1$Var1)
    open1$Freq <- as.numeric(open1$Freq)
    open1$type <- as.character(open1$type)
    
    merge.open.close <- merge(close1, open1, by = "Var1", all = TRUE)
    merge.open.close[is.na(merge.open.close)] <- 0
    merge.open.close$diff <- (merge.open.close$Freq.y - merge.open.close$Freq.x)
    merge.open.close$cumsum.diff <- cumsum(merge.open.close$diff)
    merge.open.close$Var1 <- as.Date(merge.open.close$Var1)
    
    
    
    merge2 <- merge(x=open_v_close, y = merge.open.close[, c("Var1", "diff", "cumsum.diff")], by = "Var1", all.x=TRUE)
    
    
    gvisMotionChart(merge2, "type", "Var1")
    
    
    
  })
  output$time <- renderGvis({datasetInput.aggregate()})
  datasetInput.open_vs_close <- reactive({
    #backlog rework to count open and closed tasks 
    #backlog rework to count open and closed tasks 
    current_date <- Sys.Date()
    week_ago <- Sys.Date() - 7 
    current.week.number <- strftime(current_date,format="%W")
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    year_2016 <- as.Date("2016-01-01")
    
    backlog <- getURL(paste0("http://10.124.139.237:5000/stats/query2?sdate=",input$dates[1],"&edate=", input$dates[2]))
    backlog <- fromJSON(backlog, method = "C", unexpected.escape = "error")
    backlog <- as.data.frame(backlog)
    
    
    backlog.split <- str_split_fixed(backlog$backlog, ",",3)
    backlog.split <- as.data.frame(backlog.split)
    colnames(backlog.split) <- c("tags", "open.date", "close.date")
    backlog.split2 <- str_split_fixed(backlog.split$tags, "]",3)
    backlog.split2 <- as.data.frame(backlog.split2)
    backlog.split2$V1 <- gsub('\\[',"",backlog.split2$V1)
    backlog.split2$V2 <- gsub('\\[',"",backlog.split2$V2)
    backlog.split2$V3 <- gsub('\\[',"",backlog.split2$V3)
    backlog.split2$V3 <- gsub('\\]',"",backlog.split2$V3)
    colnames(backlog.split2) <- c("severity", "bloomberg_tag", "rule")
    
    backlog.master <- cbind(backlog.split,backlog.split2)
    backlog.master$bloomberg_tag <- str_trim(backlog.master$bloomberg_tag)
    backlog.master$src <- trim(backlog.master$rule)
    backlog.master$src <- as.character(backlog.master$rule)
    backlog.master$open.date <- as.POSIXct(strptime(backlog.master$open.date, "%Y-%m-%d"))
    backlog.master$close.date <- as.POSIXct(strptime(backlog.master$close.date, "%Y-%m-%d"))
    
    
    
    #split 
    backlog.master$rule_extract <- gsub('[^ ]*$',"",backlog.master$rule)
    backlog.master$rule_extract <- gsub('\\:',"",backlog.master$rule_extract)
    backlog.master$src <- str_extract(backlog.master$rule_extract, '\\w+')
    
    
    
    backlog.master$close.week.number <- strftime(backlog.master$close.date,format="%W")
    backlog.master$close.month.number <- strftime(backlog.master$close.date,format="%m")
    backlog.master$close.month.name <- strftime(backlog.master$close.date,format="%B")
    
    
    
    backlog.master$open.week.number <- strftime(backlog.master$open.date,format="%W")
    backlog.master$open.month.number <- strftime(backlog.master$open.date,format="%m")
    backlog.master$open.month.name <- strftime(backlog.master$open.date,format="%B")
    
    open.day <- as.data.frame(table(backlog.master$open.date))
    open.day$type <- "open"
    close.day <- as.data.frame(table(backlog.master$close.date))
    close.day$type <- "close"
    
    open_v_close <- rbind(open.day, close.day)
    open_v_close <- as.data.frame(open_v_close)
    open_v_close$Var1 <- as.Date(open_v_close$Var1)
    open_v_close$Freq <- as.numeric(open_v_close$Freq)
    open_v_close$type <- as.character(open_v_close$type)
    open_v_close  <- open_v_close[order(as.Date(open_v_close$Var1, format="%Y/%m/%d")),]
    
    
    
    
    close1 <- subset(open_v_close, type == "close")
    close1 <- as.data.frame(close1)
    open1 <- subset(open_v_close, type == "open")
    open1 <- as.data.frame(open1)
    open1$cumsum <- cumsum(open1$Freq)
    close1$cumsum <- cumsum(close1$Freq)
    
    close1$Var1 <- as.Date(close1$Var1)
    close1$Freq <- as.numeric(close1$Freq)
    close1$type <- as.character(close1$type)
    
    open1$Var1 <- as.Date(open1$Var1)
    open1$Freq <- as.numeric(open1$Freq)
    open1$type <- as.character(open1$type)
    
    merge.open.close <- merge(close1, open1, by = "Var1", all = TRUE)
    merge.open.close[is.na(merge.open.close)] <- 0
    merge.open.close$diff <- (merge.open.close$Freq.y - merge.open.close$Freq.x)
    merge.open.close$cumsum.diff <- cumsum(merge.open.close$diff)
    merge.open.close$Var1 <- as.Date(merge.open.close$Var1)
    
    
    
    names(merge.open.close)[2] <- "close"
    names(merge.open.close)[5] <- "open"
    
    
    gvisAreaChart(merge.open.close, xvar="Var1", yvar=c("close","open"))
    
    
    
    
    
    
    
  })
  output$openVclose <- renderGvis({datasetInput.open_vs_close()})
  output$rpivot <- rpivotTable::renderRpivotTable({
    
    
    perf_grab <- getURL(paste0("http://10.124.139.237:5000/stats/analystTime?sdate=",input$dates[1],"&edate=",input$dates[2]))
    json_file <- fromJSON(perf_grab, method = "C", unexpected.escape = "error")
    
    
    json_file <- lapply(json_file, function(x) {
      x[sapply(x, is.null)] <- NA
      unlist(x)
    })
    
    #perf.df <- do.call("rbind", json_file)
    perf.df <- ldply(json_file, rbind)
    perf.df <- as.data.frame(perf.df)
    
    
    #split the description tags in []
    v <- str_split_fixed(perf.df$description, "]",3)
    v <- as.data.frame(v)
    v$V1 <- gsub('\\[',"",v$V1)
    v$V2 <- gsub('\\[',"",v$V2)
    v$V3 <- gsub('\\[',"",v$V3)
    colnames(v) <- c("severity", "bloomberg_tag", "log.source")
    v$log.source <- gsub('\\]',"",v$log.source)
    
    #combine the new tags
    perf.df <- cbind(perf.df,v)
    perf.df <- as.data.frame(perf.df)
    
    
    #team assignments 
    perf.df$team[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|bsingh|kbrooks|erichardson',perf.df$owners, perl=TRUE, ignore.case = TRUE)] <- "Triage"
    perf.df$team[grepl('gdemitre|blodge|rcave|croose|wwelch|ggrisamore',perf.df$owners, perl=TRUE, ignore.case = TRUE)] <- "Analysis"
    perf.df$team[grepl('bhartstein|nsimonian|mnitulescu|miacovacci|bchang|sgopinath',perf.df$owners, perl=TRUE, ignore.case = TRUE)] <- "CIRT"
    perf.df$team[grepl('vjimenez',perf.df$owners, perl=TRUE, ignore.case = TRUE)] <- "Victors"
    
    
    
    perf.df$shift[grepl('bsingh|kbrooks|',perf.df$owners,perl=TRUE, ignore.case = TRUE)] <- "2"
    perf.df$shift[grepl('vsaunders|oferguson|bpeters|jbrown|avonderlinde|erichardson',perf.df$owners,perl=TRUE, ignore.case = TRUE)] <- "1"
    perf.df$shift[grepl('bhartstein|vjimenez|nsimonian|mnitulescu|miacovacci|bchang|sgopinath|gdemitre|blodge|rcave|croose|wwelch|ggrisamore|NA',perf.df$owners,perl=TRUE, ignore.case = TRUE)] <- "0"
    
    perf.df$shift <- as.numeric(perf.df$shift)
    
    #convert to date fields 
    perf.df$assigned_ts <- as.POSIXct(perf.df$assigned_ts)
    perf.df$trusted_escalated_ts <- as.POSIXct(perf.df$trusted_escalated_ts)
    perf.df$create_ts <- as.POSIXct(perf.df$create_ts)
    perf.df$escalated_ts <- as.POSIXct(perf.df$trusted_escalated_ts)
    perf.df$completed_ts <- as.POSIXct(perf.df$completed_ts)
    
    #Response Time
    perf.df$Response.Time <- as.numeric(round(difftime(perf.df$assigned_ts, perf.df$create_ts, units="hours"),2))
    
    #Resolution Time
    perf.df$Resolution.Time <- as.numeric(round(difftime(perf.df$completed_ts, perf.df$assigned_ts, units="hours"),2))
    
    #Completed
    perf.df$completed.t.f <- !is.na(perf.df$completed_ts)
    
    #completed within the last week
    today <- as.POSIXct(Sys.Date())
    week_ago <- as.POSIXct(Sys.Date() - 7)
    two.weeks.ago <- as.POSIXct(week_ago - 7)
    perf.df$completed.week <- perf.df$completed_ts>=week_ago
    
    #stale tasks are opened less than 2 weeks ago (with a completion of FALSE)
    perf.df$stale <- perf.df$create_ts<=two.weeks.ago
    
    #get week number completed
    perf.df$completed.week.num <- strftime(perf.df$completed_ts, "%U")
    
    #get month number opened 
    perf.df$open.month.num <- strftime(perf.df$create_ts, "%m")
    
    #complted month opened
    perf.df$completed.month.num <- strftime(perf.df$completed_ts, "%m")
    
    #escalation to resolution time
    perf.df$escalated.resolution.time <- as.numeric(round(difftime(perf.df$completed_ts, perf.df$escalated_ts, units = "hours"), 2))
    
    #acknolegement to escalation
    perf.df$ack.to.escalation <- as.numeric(round(difftime(perf.df$assigned_ts, perf.df$escalated_ts, units = "hours"), 2))
    
    rpivotTable(perf.df)
  })
  
})











