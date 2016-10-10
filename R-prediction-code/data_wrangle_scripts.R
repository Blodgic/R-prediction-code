#data wrangling
#http://www.computerworld.com/article/2486425/business-intelligence-4-data-wrangling-tasks-in-r-for-advanced-beginners.html?page=6

fy <- c(2010,2011,2012,2010,2011,2012,2010,2011,2012) 
company <- c("Apple","Apple","Apple","Google","Google","Google","Microsoft","Microsoft","Microsoft") 
revenue <- c(65225,108249,156508,29321,37905,50175,62484,69943,73723) 
profit <- c(14013,25922,41733,8505,9737,10737,18760,23150,16978) 
companiesData <- data.frame(fy, company, revenue, profit)
describe(companiesData)
View(companiesData)
str(companiesData)
companiesData$fy <- as.factor(companiesData$fy)
companiesData$margin <- (companiesData$profit / companiesData$revenue) * 100
View(companiesData)
companiesData$margin <- round(companiesData$margin, 1)

companiesData <- transform(companiesData, margin = round(profit/revenue) * 100, 1)
companiesData$sums <- apply(companiesData[,c('revenue', 'profit')], 1, function(x) sum(x))
View(companiesData)
companiesData$margin <- apply(companiesData[,c('revenue', 'profit')], 1, function(x) { (x[2]/x[1]) * 100 } )
View(companiesData)

companiesData$margin <- mapply(function(x, y) round((x/y) * 100, 1), 
                               companiesData$profit, companiesData$revenue)
View(companiesData)

install.packages(dplyr)
highestMargin <- max(companiesData$margin)
highestMargin
highestMargin <- companiesData[companiesData$margin == max(companiesData$margin),]
highestMargin
highestMargin <- subset(companiesData, margin==max(margin))
highestMargin
highestProfitMargins <- ddply(companiesData, .(company), summarize, bestMargin = max(margin))
highestProfitMargins
highestProfitMargins <- ddply(companiesData, 'company', summarize, bestMargin = max(margin))
highestProfitMargins
View(companiesData)
highestProfitMargins <- ddply(companiesData, 'company', transform, bestMargin = max(margin))
View(companiesData)
highestProfitMargins
myResults <- ddply(companiesData, 'company', transform, highestMargin = max(margin), lowestMargin = min(margin))
myResults

#To add the two columns for highest and lowest margins by company:  
myresults <- companiesData %.% group_by(company) %.% mutate(highestMargin = max(margin), lowestMargin = min(margin))
myresults
highestProfitMargins <- companiesData %.% group_by(company) %.% summarise(bestMargin = max(margin))
highestProfitMargins
View(highestProfitMargins)

vDates <- as.Date(c("2013-06-01", "2013-07-08", "2013-09-01", "2013-09-15"))
vDates
VDates.month <-  cut(vDates, breaks = "month")
VDates.month
dfDates <- data.frame(vDates, VDates.month)
dfDates




companyOrder <- order(companiesData$margin)
companyOrder <- companiesData[companyOrder,]
companyOrder
companiesOrdered <- companiesData[order(companiesData$margin),]
companiesOrdered
companiesOrdered <- companiesData[order(companiesData$margin),c("fy", "company", "sums", "profit", "margin")]
companiesOrdered
companiesData[order(companiesData$fy, -companiesData$margin),]
companiesOrdered <- companiesData[with(companiesData, order(fy, -margin)),]
companiesOrdered
#using doBy
#orderBy(~columnName + secondColumnName, data=dataFrameName)
companiesOrdered <- orderBy(~-margin, companiesData)
companiesOrdered
#plyr and dplyr
#arrange(dataFrameName, columnName, secondColumnName)
companiesOrdered <- arrange(companiesData, desc(margin))
companiesOrdered

library(reshape2) 
#code to reshape a data frame from wide to long
#longData <- melt(your original data frame, a vector of your category variables)
companiesLong <- melt(companiesData, c("fy", "company"))
companiesLong
companiesLong <- melt(companiesData, id.vars=c("fy", "company"), 
                      measure.vars=c("revenue", "profit", "margin"),
                      variable.name="financialCategory", value.name="amount")
#lists all the column in the data frame, assigning them to either id.vars or 
#measure.vars, and also changes the new column names from the default "variable" and "value":
companiesLong

#wideDataFrame <- dcast(longDataFrame, idVariableColumn1 + idVariableColumn2 ~ variableColumn, 
 #                      value.var="Name of column with the measurement values")
#dcast() takes the name of a long data frame as the first argument. 
#You need to create a formula of sorts as the second argument with the syntax:
#id variables ~ variable variables
companiesWide <- dcast(companiesLong, fy + company ~ financialCategory, value.var="amount")
companiesWide
summary(companiesWide)
str(companiesWide)
