#TMA Features

#ZIP Codes
zip <- read.csv("Desktop/MSBA/TMA_features/feature_distanceto_TMA.csv", header=TRUE)
summary(zip)
View(zip)
str(zip)
summary(zip$zip.normalized)
#do count of unique zip codes from customer data
zip.count <- as.data.frame(table(zip$zip.normalized))
colnames(zip.count) <- c("zip", "freq")
View(zip.count)

#match lat and long to zipcode

library(zipcode)
data(zipcode)
zipcode <- as.data.frame(zipcode)
#merge data
#https://docs.tibco.com/pub/enterprise-runtime-for-R/1.5.0_may_2013/TERR_1.5.0_LanguageRef/base/merge.html
zip.latlong <- merge(zip.count, zipcode, by.x='zip', by.y='zip')
View(zip.latlong)
getwd()
setwd("Desktop/MSBA/TMA_features/")
write.csv(zip.latlong, file= 'ziplatlong_TMA.csv')
#merge Josh's original zip file
zip.original <-merge(zip, zipcode, by.x='zip.normalized', by.y='zip')
View(zip.original)
write.csv(zip.original, file= 'ziporiginal_TMA.csv')
#http://blogs.splunk.com/2013/07/31/zip-code-mapping/
#-Splunk-
#|inputlookup ziporiginal_TMA.csv | geonormalize zip

#median/mean zip to income levels
#MeidanZIP.csv from http://www.psc.isr.umich.edu/dis/census/Features/tract2zip/ 
zip.income <- read.csv("MedianZIP.csv", header=TRUE)
zip.income <- as.data.frame(zip.income)
zip.income.merge <- merge(zip.original, zip.income, by.x='zip.normalized', by.y='Zip')
View(zip.income.merge)
