#web scrapting basics
#http://quantifyingmemory.blogspot.co.uk/2014/02/web-scraping-basics.html

require(ggplot2)
clubs <- c("Tottenham","Arsenal","Liverpool",
           "Everton","ManU","ManC","Chelsea")
nPages <- c(23.3,68.4,78.9,35.5,102,90.5,110)
df <- data.frame(clubs,nPages)
df
ggplot(df,aes(clubs,nPages,fill=clubs))+
  geom_bar(stat="identity")+
  coord_flip()+theme_bw(base_size=70)

var=123
paste("url",var,sep="")
#[1] "url123"
paste("url",var,sep=" ")
#[1] "url 123"

var=123
paste("url",rep(var,3),sep="_")
#[1] "url_123" "url_123" "url_123"
paste(rep("url",3),var,sep="_")
#[1] "url_123" "url_123" "url_123"
var=c(123,421)
paste(var,collapse="_")
#[1] "123_421"

#Task using 'paste'
a='test'
b='scrape'
c=94
#merge variables a,b,c into a string, separated by an underscore (“_”)
var=c('test','scrape',94)
var=c(a,b,c)
var1=(1:10)
paste(var, collapse="")
paste(rep('a',10))
paste(rep('a',10),var1,sep="")


var=201401
url=paste('http://stats.grok.se/json/en/',var,'/web_scraping',sep='')
url
browseURL(url)

#fetching data
var=201401
url=paste("http://stats.grok.se/json/en/",var,"/web_scraping",sep="")
raw.data <- readLines(url, warn="F")
raw.data

install.packages("rjson")
require(rjson)
rd <- fromJSON(raw.data)
rd

rd.views <- rd$daily_views
rd.views
rd.views <- unlist(rd.views)
df <- as.data.frame(rd.views)
df

rd <- fromJSON(readLines(url, warn="F"))
rd.views <- rd$daily_views
df <- as.data.frame(unlist(rd.views))
df

require(ggplot2)
require(lubridate)
df$date <-  as.Date(rownames(df))
colnames(df) <- c("views","date")
ggplot(df,aes(date,views))+
  geom_line()+
  geom_smooth()+
  theme_bw(base_size=20)

#loops
plusOne <- function(x){
  return(x+1)
}
plusOne(8)
plusOne2 <- function(num){
  return(num+1)
}
plusOne2(10)

for (number in 1:5){
  print(number)
}
a <- c(1,2,3,4,5)
for (value in a){
  print(
    plusOne(value)
    )
}

listofNumbers <- c(1,2,3,4,5)
for (number in listofNumbers){
  print (
    number + 1)
}

a <- c(1,2,3,4,5)
a[1]
a[4]
for(i in 1:length(a)){
  print (
    plusOne(a[i])
    )
}
print(1:length(a))
print (length(a))
print(a)
a <- c(1,2,3,4,5)
a+1
plusOne(a)
#Can be used in all sorts of situations, 
#slow - similar to a loop, better if you are colleting an output
sapply(a, plusOne)

for (month in 1:12){
  print(paste(2014,month,sep=""))
}

for (month in 1:9){
  print(paste(2012,0,month,sep=""))
}
for (month in 10:12){
  print(paste(2012,month,sep=""))
}
dates=NULL
for (month in 1:9){
  date=(paste(2012,0,month,sep=""))
  dates=c(dates,date)
}
for (month in 10:12){
  date=paste(2012,month,sep="")
  date=c(dates,date)
}
print (as.numeric(dates))
print (as.numeric(date))
#!! To do this with a data.frame, use rbind()

for (year in 2012:2013){
  for (month in 1:9){
    print(paste(year,0,month,sep=""))
  }
  for (month in 10:12){
    print(paste(year,month,sep=""))
  }
}

#pulled together
for (year in 2012:2013){
  for (month in 1:9){
    print(paste("http://stats.grok.se/json/en/",year,0,month,"/web_scraping",sep=""))
  }
  for (month in 10:12){
    print(paste("http://stats.grok.se/json/en/",year,month,"/web_scraping",sep=""))
  }
}

getData <- function(url){
  raw.data <- readLines(url, warn="F") 
  rd  <- fromJSON(raw.data)
  rd.views <- rd$daily_views 
  rd.views <- unlist(rd.views)
  rd <- as.data.frame(rd.views)
  rd$date <- rownames(rd)
  rownames(rd) <- NULL
  return(rd)
}
#web scraping part 2
wage=20000
taxFree=9400
rate=20
(wage-taxFree)*rate/100

printname <- function(){
  print ("My name is Brennan")
}
printname()

#simulation
sillSimulation <- function(){
  x1 <- runif(500,80,100)
  x2 <- runif(500,0,100)
  v1 <- c(x1,x2)
  x3 <- runif(1000,0,100)
  df <- data.frame(v1,x3)
  require(ggplot)
  print(ggplot(df, aes(v1,x3))+geom_point()+ggtitle("simulation of some sort"))
}
sillSimulation()

desperateTimes <- function(name){
  print(paste0(name," is struggling to finish his PhD on time. 
               Time remaining: 6months"))
}
desperateTimes(name = "Tom")

desperateTimes(name="Tanya",gender="f")

desperateTimes <- function(name,gender="m"){
  if(gender=="m"){
    pronoun="his"
  }else{
    pronoun="her"
  }
  
  print(paste0(name ," is struggling to finish ",pronoun," PhD on time. Time remaining: 6 months"))
}
desperateTimes(name="Tanya",gender="f")

require(lubridate)
require(ggplot2)
deadline=as.Date("2014-09-01")
daysleft <- deadline-Sys.Date()
totDays <- deadline-as.Date("2011-10-01")
print(daysleft)

print(paste0("Percentage to go: ",round(as.numeric(daysleft)/as.numeric(totDays)*100)))
df <- data.frame(days=c(daysleft,totDays-daysleft),lab=c("to go","completed"))
ggplot(df,aes(1,days,fill=lab))+geom_bar(stat="identity",position="fill")
                                                          
timeToWorry <- function(){
  require(lubridate)
  deadline=as.Date("2014-09-01")
  daysLeft <- deadline-Sys.Date()
  totDays <- deadline-as.Date("2011-10-01")
  print(daysLeft)
  print(paste0("Rolf is struggling to finish his PhD on time. Days remaining: ", as.numeric(daysLeft)))
  print(paste0("Percentage to go: ",round(as.numeric(daysLeft)/as.numeric(totDays)*100)))
  df <- data.frame(days=c(daysLeft,totDays-daysLeft),lab=c("to go","completed"))
  ggplot(df,aes(1,days,fill=lab))+geom_bar(stat="identity",position="fill")
}
timeToWorry()
