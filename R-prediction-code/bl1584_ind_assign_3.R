library(forecast)
library(plyr)
library(tseries)
library(plyr)
library(ddply)

#Energy Assignment 3 
Ontario <- read.csv(file.choose(), sep = ",", fill = TRUE)

#histogram of the deamnd 
hist(Ontario$Ontario.Demand)

#Average per day
Ontario.Mean <-with(Ontario, 
                    tapply(Ontario$Ontario.Demand, Ontario$Date, mean))

Ontario.Mean <- as.data.frame(Ontario.Mean)
Ontario.Mean1 <- merge(Ontario.Mean, Ontario, by.x='row.names', by.y='Date')
View(Ontario.Mean)


#Standard Deviation per day
Ontario.sd <-with(Ontario, 
                  tapply(Ontario$Ontario.Demand, Ontario$Date, sd))
Ontario.sd <- as.data.frame(Ontario.sd)
View(Ontario.Mean)
plot(Ontario.sd)

#Set date formats
#http://stackoverflow.com/questions/7439977/changing-date-format-in-r
Ontario$Date <- format(Ontario$Date, format="%d-%b-%y")
Ontario$Date.f <- strptime(as.character(Ontario$Date), "%d-%b-%y")
format(Ontario$Date.f, "%Y-%m-%d")
Ontario$Date.f <- as.Date(Ontario$Date.f)
str(Ontario)
#move Date.f to first column
Ontario <- Ontario[,c(5,1,2,3,4)]


#Create the test data
# Use the dataset to forecast (using time series) 
#the daily demand pattern following the steps
#below:
#a. Select your hold out sample as the last month of the observations
# hold out sample will equal 720 hours (30 days) of data
#b. Estimate your ARIMA or ARMA model parameters on the rest of your data set

testdata <- ts(Ontario$Ontario.Demand, frequency=24)
#number of rows in data set
length(testdata)
#subtract 30days or 720hrs from last row
length(testdata) - 720
#rows 15663 - 16383 will be the holdout data 
testdata.holdout <- testdata[15663:16383]
#rows 1-15662 will be other data
testdata.other <- testdata[1:15662]

#ts the holdout data
testdata.holdout.ts <- ts(testdata.holdout, frequency=24)
plot(testdata.holdout.ts)

#ts the other data (non-holdout) & Seasonal Data
testdata.other.ts <- ts(testdata.other, frequency=24)
fit.seasonal <- stl(testdata.other.ts, s.window="period")
plot(testdata.other.ts)
plot(fit.seasonal)

#use auto.arima forecast function to predict a forecast for test data against holdout data
#and use auto.arima to find the best fit
#http://www.inside-r.org/packages/cran/forecast/docs/auto.arima
fit <- auto.arima(testdata.other.ts)
#use 30 as 30 days to forecast ahead of the original 30days of data
fit.arima <- forecast(fit, 30)
plot.forecast(fit.arima)

#results arima
fit.arima

#accuracy of arima predictions
View(accuracy(fit.arima))

#plotting ARMA using the auto.arima order of (2,1,0,2,0,0)
#http://lojze.lugos.si/~darja/software/r/library/tseries/html/arma.html

summary(fit.arma <- arma(testdata.other.ts, order=c(2,1)))
accuracy(fit.arma)
fit.arma <- predict(fit.arma, n.ahead=30)
fit.arma.p <- predict(fit.arma, n.ahead=30)


#plot the arma fucntions
#http://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html
tsdisplay(testdata.other.ts)
acf(testdata.other.ts)
pacf(testdata.other.ts)




