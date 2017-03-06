#importing dataset
#data <- read_excel("Shipment Data.xlsx")
data <- Shipment_Data
#finding class
#class(data)

#slicing time (year-week) and units alone
dataDryer <- data[data$PRODUCT == "Top Mount",c(3,5)]

plot(dataDryer$INDUSTRY_UNITS,dataDryer$WEEK)
#converting the dataframe to timeseries
DryerTs <- ts(dataDryer$INDUSTRY_UNITS, start=c(2010,1), end= c(2015,26), freq=54)

#finding class
class(DryerTs)

#plot the time series
ts.plot(diff(DryerTs))

#Augmented Dickey-Fuller Test for given series
#adf.test(DryerTs)

#acf graph for given series
#acf(DryerTs)

#pacf graph for given series
#pacf(DryerTs)

#Augmented Dickey-Fuller Test after differencing
#adf.test(diff(DryerTs))

#acf graph after differencing
acf2(diff(DryerTs))
acf2(diff(DryerTs), max.lag=200)
#pacf graph after differencing
#pacf(diff(DryerTs))

#fitting ARIMA model
fitDryer <- arima(DryerTs, c(1,0,1),seasonal = list(order = c(0,0,1), period = 54))

#predicting for next 2 quarters (27 weeks)
predDryer <- predict(fitDryer, n.ahead=27)

#actual value(sales) for Q3 and Q4 (taken from actual data for evaluation)
actual <- 2478801


#predicted value (demand) for Q3 and Q4                    
predicted1 <- sum(predDryer$pred[1:13]) + predDryer$pred[14]/2
predicted2 <- sum(predDryer$pred[15:27]) + predDryer$pred[14]/2
predicted1
predicted2

#Accuracy                     
abs((actual-predicted1-predicted2)*100)/actual -100

#plotting the graph along with forecated value for Q3 and Q4 2015
#ts.plot(DryerTs, predDryer$pred, lty = c(1,3))

