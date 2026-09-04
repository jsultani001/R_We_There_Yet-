
###QUESTION 1###

###READ THE FILE
Appliance.data<-read.csv("C:/Users/josep/Documents/ODU Spring 2026/PREDICTIVE ANALYTICS/HW1 Files/ApplianceShipments.csv")

###IDENTIFY TIME SERIES
shipments.ts<-ts(Appliance.data$shipments,start=c(1985,1), frequency = 3)

###PLOT THE TIME SERIES
plot(shipments.ts,ylab="Shipments-(millions USD)")




###QUESTION 2###



###READ THE FILE
Sept11.data<-read.csv("C:/Users/josep/Documents/ODU Spring 2026/PREDICTIVE ANALYTICS/HW1 Files/Sept11Travel-1.csv")

###IDENTIFY TIME SERIES
rail.ts<-ts(Sept11.data$rail,start=c(1990,1),end=c(2004,4),frequency = 12)
vmt.ts<-ts(Sept11.data$vmt,start=c(1990,1),end=c(2004,4),frequency = 12)
air.ts<-ts(Sept11.data$air,start=c(1990,1),end=c(2004,4),frequency = 12)


par(mfrow=c(3,1))

###PLOT TIME SERIES
plot(rail.ts,ylab="Rail Passenger Miles")
plot(vmt.ts,ylab="Vehicle Miles")
plot(air.ts,ylab="Air Passenger Miles")


Rail.PrEvent<- window(rail.ts,start=c(1990,1),end=c(2001,9))
Rail.PoEvent<- window(rail.ts,start=c(2001,9),end=c(2004,4))
lines(Rail.PrEvent, col="blue", lwd=2)
lines(Rail.PoEvent, col="red", lwd=2)

Vmt.PrEvent<- window(vmt.ts,start=c(1990,1),end=c(2001,9))
Vmt.PoEvent<- window(vmt.ts,start=c(2001,9),end=c(2004,4))
lines(Vmt.PrEvent, col="blue", lwd=2)
lines(Vmt.PoEvent, col="red", lwd=2)

Air.PrEvent<- window(air.ts,start=c(1990,1),end=c(2001,9))
Air.PoEvent<- window(air.ts,start=c(2001,9),end=c(2004,4))
lines(Air.PrEvent, col="blue", lwd=2)
lines(Air.PoEvent, col="red", lwd=2)


###QUESTION 3###


souvenir.data<-read.csv("C:/Users/josep/Documents/ODU Spring 2026/PREDICTIVE ANALYTICS/HW1 Files/SouvenirSales.csv")

Asales.ts<-ts(souvenir.data$sales,start=c(1995,1),end=c(2001,12),frequency = 12)
Bsales.ts<-ts(souvenir.data$sales,start=c(1997,1),end=c(1999,12),frequency = 12)

options(scipen = 999)

par(mfrow=c(2,1))


plot(Asales.ts,ylab=("Sales(1995-2001)"),ylim=c(1660,105000))
plot(Bsales.ts,ylab=("Sales(1997-1999)"),ylim=c(1660,105000))