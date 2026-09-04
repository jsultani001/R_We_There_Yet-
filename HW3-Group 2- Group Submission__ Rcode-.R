library(forecast)
library(zoo)
###QUESTION 1###

###READ THE FILE
Appliance.data<-read.csv("C:/Users/josep/Documents/ODU Spring 2026/PREDICTIVE ANALYTICS/HW1 Files/ApplianceShipments.csv")

###IDENTIFY TIME SERIES
shipments.ts<-ts(Appliance.data$shipments,start=c(1985,1),end=c(1989,4), frequency = 4)

###PLOT THE TIME SERIES
plot(shipments.ts,ylab="Shipments-(millions USD)",ylim=c(3900,5000), bty="l")

nvalid.shipments<-4
ntrain.shipments<-length(shipments.ts)-nvalid.shipments

train.shipments.ts <-window(shipments.ts, start=c(1985,1), end=c(1985,ntrain.shipments))
valid.shipments.ts <- window(shipments.ts, start=c(1985,ntrain.shipments+1), end=c(1985,ntrain.shipments+nvalid.shipments))

lines(train.shipments.ts,col="blue")
lines(valid.shipments.ts,col="red")

###LINEAR TREND###
train.shipments.lm<-tslm(train.shipments.ts~trend)
train.shipments.lm.pred<-forecast(train.shipments.lm)
lines(train.shipments.lm.pred$fitted,col="red",lwd=2)
lines(train.shipments.lm.pred$mean,col="red",lwd=2,lty=2)
accuracy(train.shipments.lm.pred,valid.shipments.ts)

###EXPONENTIAL TREND###
train.shipments.exp<-tslm(train.shipments.ts~trend, lambda=0)
train.shipments.exp.pred<-forecast(train.shipments.exp, h=nvalid.shipments)
lines(train.shipments.exp.pred$fitted, col="red",lwd=2)
lines(train.shipments.exp.pred$mean,col="red",lwd=2,lty=2)
accuracy(train.shipments.exp.pred,valid.shipments.ts)

###POLYNOMIAL (QUADRATIC TREND)###
train.lm.shipments.poly<-tslm(train.shipments.ts~trend+I(trend^2))
train.lm.shipments.poly.pred<-forecast(train.lm.shipments.poly, h=nvalid.shipments)
lines(train.lm.shipments.poly.pred$fitted, col="red",lwd=2)
lines(train.lm.shipments.poly.pred$mean, col="red",lwd=2,lty=2)
accuracy(train.lm.shipments.poly.pred,valid.ts)

###MODELING SEASAONALITY###
train.lm.shipments.season<-tslm(train.shipments.ts~season)
train.lm.shipments.season.pred<-forecast(train.lm.shipments.season, h=nvalid.shipments)
lines(train.lm.shipments.season.pred$fitted,col="red",lwd=2)
lines(train.lm.shipments.season.pred$mean,col="red",lwd=2,lty=2)
accuracy(train.lm.shipments.season.pred,valid.ts)

###POLYNOMIAL (QUADRATIC TREND) W/ SEASONALITY###
train.shipments.poly.season<-tslm(train.shipments.ts~trend+I(trend^2)+season)
train.shipments.poly.season.pred<-forecast(train.shipments.poly.season,h=nvalid)
lines(train.shipments.poly.season.pred$fitted,col="red",lwd=2)
lines(train.shipments.poly.season.pred$mean,col="red",lwd=2,lty=2)
accuracy(train.shipments.poly.season.pred,valid.ts)



###QUESTION 2###


souvenir.data<-read.csv("C:/Users/josep/Documents/ODU Spring 2026/PREDICTIVE ANALYTICS/HW1 Files/SouvenirSales2.csv")

sales.ts<-ts(souvenir.data$sales,start=c(1995,1),end=c(2001,12),frequency = 12)

plot(sales.ts,ylab="Sales",ylim=c(1500,110000),bty="l")
options(scipen = 999)

nvalid.sales<-12
ntrain.sales<-length(sales.ts)-nvalid.sales

train.sales.ts<-window(sales.ts,start=c(1995,1),end=c(1995,ntrain.sales))
valid.sales.ts<-window(sales.ts,start=c(1995,ntrain.sales+1),end=c(1995,ntrain.sales+nvalid.sales))

lines(train.sales.ts,col="blue")                       
lines(valid.sales.ts,col="red")

###LINEAR TREND###
train.sales.lm<-tslm(train.sales.ts~trend)
train.sales.lm.pred<-forecast(train.sales.lm)
lines(train.sales.lm.pred$fitted,col="red",lwd=2)
lines(train.sales.lm.pred$mean,col="red",lwd=2,lty=2)
accuracy(train.sales.lm.pred,valid.sales.ts)

###EXPONENTIAL TREND###
train.sales.exp<-tslm(train.sales.ts~trend, lambda=0)
train.sales.exp.pred<-forecast(train.sales.exp, h=nvalid.sales)
lines(train.sales.exp.pred$fitted, col="red",lwd=2)
lines(train.sales.exp.pred$mean,col="red",lwd=2,lty=2)
accuracy(train.sales.exp.pred,valid.sales.ts)

###POLYNOMIAL (QUADRATIC TREND)###
train.lm.sales.poly<-tslm(train.sales.ts~trend+I(trend^2)+I(trend^3))
train.lm.sales.poly.pred<-forecast(train.lm.sales.poly, h=nvalid.sales)
lines(train.lm.sales.poly.pred$fitted, col="red",lwd=2)
lines(train.lm.sales.poly.pred$mean, col="red",lwd=2,lty=2)
accuracy(train.lm.sales.poly.pred,valid.sales.ts)

###LINEAR TREND W/ SEASONALITY###
train.sales.lm.season<-tslm(train.sales.ts~trend+season)
train.sales.lm.season.pred<-forecast(train.sales.lm.season,h=nvalid.sales)
lines(train.sales.lm.season.pred$fitted,col="red",lwd=2)
lines(train.sales.lm.season.pred$mean,col="red",lwd=2,lty=2)
accuracy(train.sales.lm.season.pred,valid.sales.ts)

###POLYNOMIAL (QUADRATIC TREND) W/ SEASONALITY###
train.sales.poly.season<-tslm(train.sales.ts~trend+I(trend^2)+season)
train.sales.poly.season.pred<-forecast(train.sales.poly.season,h=nvalid.sales)
lines(train.sales.poly.season.pred$fitted,col="red",lwd=2)
lines(train.sales.poly.season.pred$mean,col="red",lwd=2,lty=2)
accuracy(train.sales.poly.season.pred,valid.sales.ts)


