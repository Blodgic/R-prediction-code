rm(list = ls())

library(psych)
library(ggplot2)
library(tseries)
library(stats)
library(lubridate)
library(forecast)


#od <- read.csv("/Users/jamieweaver/Dropbox//NYU mod4/Energy Analytics/OntarioDemand.csv")
od <- read.csv("C:/JW/Dropbox/NYU mod4/Energy Analytics/OntarioDemand.csv")

dim(od)
length(unique(od$Date))
length(unique(od$Hour))

od$Hour <- as.character(od$Hour)
od$datetime <- paste(od$Date, od$Hour, sep=" ")
od$datetime <- as.POSIXct(od$datetime, format="%d-%b-%y %H")
summary(od$datetime)

od$Date <- as.Date(od$Date, "%d-%b-%y")
describe(od$Ontario.Demand)
ks.test(log(od$Ontario.Demand), "pnorm", mean(od$Ontario.Demand), sd(od$Ontario.Demand))

od.mean <- aggregate(Ontario.Demand ~ Date, od, mean)
names(od.mean) <- c("Date", "od.mean")
od.sd <- aggregate(Ontario.Demand ~ Date, od, sd)
names(od.sd) <- c("Date", "od.sd")
od.sd <- od.sd[2]
od.agg <- data.frame(od.mean, od.sd)
rm(od.mean, od.sd)

plot(od.agg$Date, od.agg$od.mean, type="l", xlab="Date", ylab="Mean daily energy demand (kWh)")
plot(od.agg$Date, od.agg$od.sd, type="l", xlab="Date", ylab="Daily energy demand standard deviation")

h <- hist(od$Ontario.Demand, breaks=50, col="gray", main=NULL, xlab="Ontario energy demand (kWh)")
xfit <- seq(min(od$Ontario.Demand), max(od$Ontario.Demand), length=50)
yfit <- dnorm(xfit, mean=mean(od$Ontario.Demand), sd=sd(od$Ontario.Demand))
yfit <- yfit*diff(h$mids[1:2])*length(od$Ontario.Demand) 
lines(xfit, yfit, lwd=2)

od.ts <- ts(od.agg[, 2], start(2002, 120), frequency=24)
head(od.ts)
plot.ts(od.ts[, c(2:3)], plot.type="single", ylab="Ontario energy demand (kWh)", xlab="Date")

fit <- stl(od.ts[, 2], s.window="period")
summary(fit)
plot(fit)

adf.test(od.ts[, 2])
tsdisplay(od.ts[, 2])
tsdisplay(diff(od.ts[, 2], differences=1))


plot(od.agg$od.mean, type="l", col="grey")
od.1 <- filter(od.agg$od.mean, filter=rep(1/5,5))
od.2 <- filter(od.agg$od.mean, filter=rep(1/25,25))
od.3 <- filter(od.agg$od.mean, filter=rep(1/81,81))
lines(od.1,col="red")
lines(od.2,col="purple")
lines(od.3,col="blue")


#strong seasonal patterns
arima.fit1 <- arima(od.agg[1:652, 2], order=c(0, 1, 1), seasonal = list(order = c(0, 1, 1)), method="ML")
arima.fit2 <- arima(od.agg[1:652, 2], order=c(0, 1, 2), seasonal = list(order = c(0, 1, 1)), method="ML")

arima.fit3 <- arima(od.agg[1:652, 2], order=c(1, 0, 0), seasonal = list(order = c(0, 1, 1)), method="ML")
arima.fit4 <- arima(od.agg[1:652, 2], order=c(2, 0, 0), seasonal = list(order = c(0, 1, 1)), method="ML")
arima.fit5 <- arima(od.agg[1:652, 2], order=c(3, 0, 0), seasonal = list(order = c(0, 1, 1)), method="ML")

arima.fit <- arima(od.agg[1:652, 2], 
                   order=c(1, 0, 1), 
                   seasonal = list(order = c(0, 1, 1)),
                   method="ML") #try other fitting methods - ML lowest RMSE

arima.pred <- predict(arima.fit, n.ahead=31)

accuracy(arima.pred$pred, od.agg[1:652, 2])





arma.fit <- arma(od.agg[1:652, 2], order=c(1,1))
accuracy(arma.fit)

arma.pred <- predict(arma.fit, n.ahead=31)








plot(forecast(arima.value), main = "Autoregressive integrated moving average forecast", xlab = "Number of days back used for prediction", ylab = "Closing price, $")


arima.value <- arima(tail(quotesym[, 4], input$period), order=c(1, 0, 1), method="ML")
plot(forecast(arima.value), main = "Autoregressive integrated moving average forecast", xlab = "Number of days back used for prediction", ylab = "Closing price, $")



plot(stl(log(od.agg$od.mean), s.window="periodic"))


summary(od)
View(od)



aggregate.plot(od$Ontario.Demand, by=list(DATE=od$Date), FUN="mean", error="sd")


ggplot(od)

od <- as.xts(od)

describe(od$Ontario.Demand)

















