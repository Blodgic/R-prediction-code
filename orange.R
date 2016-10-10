require(dplyr)   # Data Manipulation routines
require(ggplot2) # Pretty graphs
library(coefplot)
library(gridExtra)




# Read Data and Crete log of Sales & Price ofr each brand 

oj <- read.csv("C:/teach/data driven strategy/2014/oj/oj_competition.csv")

week<-rep(1:116)
logtrsa<-log(oj$TropSales)
logflsa<-log(oj$FolridaSales)
logmmsa<-log(oj$MMSales)
logplsa<-log(oj$PLSales)

logprtr<-log(oj$PriceTrop)
logprfl<-log(oj$PriceFlorida)
logprmm<-log(oj$PriceMM)
logprpl<-log(oj$PricePL)

orange<-cbind(week,logtrsa,logflsa,logmmsa,logplsa,logprtr,logprfl,logprmm,logprpl)
orange<-data.frame(orange)

fit1<-lm(logtrsa~logprtr+logprfl+logprmm+logprpl ,data=orange)
summary(fit1)
coef1<-coefplot(fit1,title="Price Elasticity of Tropicana")

fit2<-lm(logflsa~logprtr+logprfl+logprmm+logprpl ,data=orange)
summary(fit2)
coef2<-coefplot(fit2,title="Price Elasticity of Florida")

fit3<-lm(logmmsa~logprtr+logprfl+logprmm+logprpl ,data=orange)
summary(fit3)
coef3<-coefplot(fit3,title="Price Elasticity of Minute Made")

fit4<-lm(logplsa~logprtr+logprfl+logprmm+logprpl ,data=orange)
summary(fit4)
coef4<-coefplot(fit4,title="Price Elasticity of Private Label")

# Plot Elasticity for All Brands


grid.arrange(coef1,coef2,coef3,coef4, main = "Price Elasticity Coefficients", nrow = 2,  ncol = 2)
# Price Elasticity Matrix
Tropicana<-coef(fit1)[2:5]
Florida<-coef(fit2)[2:5]
MinuteMaid<-coef(fit3)[2:5]
PrivateLabel<-coef(fit4)[2:5]
prela<-rbind(Tropicana,Florida,MinuteMaid,PrivateLabel)

vulnerability<-c(0.806, 3.786, 3.086, 2.099)
clout<-c(2.141,2.91,3.616,1.29,0)

prela1<-cbind(prela, vulnerability)
prela2<-rbind(prela1, clout)
prela2





