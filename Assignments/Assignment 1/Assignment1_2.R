setwd("C:/Users/iGuest/Desktop/University of Washington/Quarter 2/INFX 573/Assignments")
#Read the data
library(dplyr)
library(ggplot2)
data <- read.csv("FelixHernandez2015.csv", stringsAsFactors=FALSE)
data2 <- read.csv("FelixHernandez2015.csv", stringsAsFactors=FALSE)

#Q1:  Calculate number of wins
sum(data$W)

#Q2: Calculate mean median & mode
mode <-function(x) 
{
  ux <-unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mean(data$SO)
median(data$SO)
mode(data$SO)

#Q3: Plot relationship between
ggplot(data, aes(x = data$IP, y = data$SO)) + geom_point()
ggplot(data, aes(x = data$IP, y = data$BB)) + geom_point()

#Q4: Correlation between innings & strikeouts and innings & walks.
corel.innings_strikeouts <- cor(data$IP, data$SO)
corel.innings_walks <- cor(data$IP, data$BB)

#Q5: Calculate the mean and variance of walks by month (hint: use the by() function like in lab).
#Do you see changing mean walks over time? 
#What about the variability over time? What might the pattern mean?
plot(by(data$BB, data$Month, mean))

#Q6: Does Felix win more on the road or at home?
wins.ontheroad <- filter(data, data$W==1, data$away==1)
wins.athome <- filter(data, data$W==1, data$away==0)
print(nrow(wins.athome) > nrow(wins.ontheroad))
