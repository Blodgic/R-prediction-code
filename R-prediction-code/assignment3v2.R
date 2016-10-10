library(forecast)
library(plyr)
library(tseries)


demand <- read.csv("C:/Users/watersma/Desktop/0 Stern MSBA/4.2 energy analytics/OntarioDemand.csv")
freq <- 24
days.mon <- 28

#Plot the raw demand data
plot(demand$Ontario.Demand)
hist(demand$Ontario.Demand)

#Change date as factor to R-date format
#Antiipated this would be used for plotting
demand$Date2 <- as.Date(demand$Date,"%d-%b-%y")

#Calculate the mean and stdev for each day and plot the stdev
daily.metrics <- ddply(demand,~Date2,summarise,mean=mean(Ontario.Demand),sd=sd(Ontario.Demand))
plot(daily.metrics$sd~daily.metrics$Date2)

#Convert data to time series and create training and testing sets
myts <- ts(demand$Ontario.Demand, frequency = freq)  #for predicitng each hour of a day
#myts <- ts(daily.metrics$mean, frequency=7)  #for predicting the daily average

myts.len <- length(myts)/freq
myts.tr.end <- (myts.len - days.mon)

myts.train <- window(myts, start=1, end=myts.tr.end)
myts.test <- window(myts, start=myts.tr.end+1, end=myts.len)
plot(window(myts, start=myts.tr.end-28, end=myts.len))


#Decompose to look for seasonalityu
fit <- stl(myts.test,s.window="period")
plot(fit)

#Test for stationary and visually evaluate ACF and PACF
#Slow erosion of ACF indicates differencing is likely needed
#ACF and PACF indicative of lag of 7
adf.test(myts)
tsdisplay(myts)
tsdisplay(diff(myts,differences=1))


#Try autofitting the ARIMA
# the best fit arima model using AIC has values of (1,0,1)(2,0,0)
fit <- auto.arima(myts.train)
fcast <- forecast(fit,days.mon)
plot(fcast)

#Experimentally evaluate values of p,d,q using RMSE and MAPE
#Limits set to be inclusive of 7-days, based on visual evaluation

max.val <- 3

RMSE <- rep(NA,max.val^6)
MAPE <- rep(NA,max.val^6)
count <- 1

for (p in 0:(max.val-1))
{
  for (d in 0:(max.val-1))
  {
    for (q in 0:(max.val-1))
    {
      for (i in 0:(max.val-1))
      {
        for (j in 0:(max.val-1))
        {
          for (k in 0:(max.val-1))
          {
            tryCatch(
            {
              fit <- arima(myts.train, order=c(p,d,q),seasonal = list(order = c(i, j, k)))
              pred <- predict(fit, n.ahead=days.mon*7)
              print(count)
              RMSE[count] <- sqrt(mean((pred$pred - myts.test)^2))
              print(RMSE[count])
              MAPE[count] <- mean((myts.test-pred$pred)/myts.test)
            }, error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
            count <- count +1            
                        
          }
        }
      }

    }
  }
}

test <- data.frame(p=rep(0:(max.val-1),each=max.val^5), 
                   d=rep(0:(max.val-1),each=max.val^4,times=max.val), 
                   q=rep(0:(max.val-1),each=max.val^3,times=max.val^2), 
                   i=rep(0:(max.val-1),each=max.val^2,times=max.val^3), 
                   j=rep(0:(max.val-1),each=max.val,times=max.val^4), 
                   k=rep(0:(max.val-1),max.val^5))
res <- cbind(test,RMSE,MAPE)
res.f <- res[complete.cases(res),]

#Show results for best auto fit
res.f[which(res.f$p==1&res.f$d==0&res.f$q==1&res.f$i==2&res.f$j==0&res.f$k==0),]

#Show results for lowest RMSE
res.f[which(res.f$RMSE==min(res.f$RMSE)),]

#Show results for lowest MAPE
res.f[which(abs(res.f$MAPE)==min(abs(res.f$MAPE))),]

# Plot the forecasts of the best fits compared to the test data
fit2 <- arima(myts.train, order=c(1,0,1), seasonal = list(order = c(2, 0, 0)))
pred2 <- predict(fit2, n.ahead=days.mon*7)

fit3 <- arima(myts.train, order=c(0,1,2), seasonal = list(order = c(2,2,1)))
pred3 <- predict(fit3, n.ahead=days.mon*7)

fit4 <- arima(myts.train, order=c(0,1,1), seasonal = list(order = c(0,0,0)))
pred4 <- predict(fit4, n.ahead=days.mon*7)

plot(myts.test)
lines(pred2$pred, col=2)
lines(pred3$pred, col=3)
lines(pred4$pred, col=4)

