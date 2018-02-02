#Prem Shah
#1728562
#Lab 1
setwd("C:/Users/iGuest/Desktop/University of Washington/Quarter 2/INFX 573/Labs/Lab 1")
library(dplyr)
seatbelts <- read.csv("seatbelts.csv")
class(seatbelts)
dim(seatbelts)
colnames(seatbelts)
summary(seatbelts)

mean(seatbelts[,"DriversKilled"])
#122.8021

mean(seatbelts[seatbelts[,"year"]>=1969 &
                 seatbelts[,"year"]<1970,"DriversKilled"])
#103

by(seatbelts[,"DriversKilled"], seatbelts[,"year"], mean)
#What was the average number of fatalities in 1970?
#122.4167
#What was the average number of fatalities in 1970?
#125.5833

by(seatbelts[,"rear"], seatbelts[,"year"]==1972, mean)
#440.25
by(seatbelts[,"rear"], seatbelts[,"year"]==1980, mean)
#380.8333
cormat = cor(seatbelts)
plot(seatbelts[,"kms"], seatbelts[,"drivers"], type = "h")
#It appears that as the number of kilometers increase, the number of deaths decreases. This might be due to more experience in driving.

plot(unique(seatbelts$year), by(seatbelts[,"DriversKilled"], seatbelts[,"year"], sum), type="l", xlab = "year", ylab = "No. of deaths")
#This plot shows that after the law was passsed in 1983 the no. of drivers killed showed a decreasing trend
plot(seatbelts[,"PetrolPrice"], seatbelts[,"DriversKilled"])
#This plot slightly shows that as the petrol price increased sometimes the number of deaths decreased     
