#Load data
fraud <- read.csv("/Users/scdavis6/Fraud/BoaAir.csv", sep = ",", na.strings = "",
                    header = TRUE)
#Data prep
summary(fraud)
str(fraud)
#Convert to time series objects
#Convert date column from factor to date object
fraud$FIN.TRANSACTION.DATE <- as.Date(fraud$FIN.TRANSACTION.DATE, "%m/%d/%y")
#Convert numeric and date to xts objects
library(xts)
fraud.xts <- xts(fraud$FIN.TRANSACTION.AMOUNT, fraud$FIN.TRANSACTION.DATE)


#Visualizations
#Histogram
hist(fraud$FIN.TRANSACTION.AMOUNT)
#Boxplot
boxplot(fraud$FIN.TRANSACTION.AMOUNT, range = 500)
#Toooo many outliers
boxplot.stats(fraud$FIN.TRANSACTION.AMOUNT)$out
#Time series plot - don't use
plot(fraud$FIN.TRANSACTION.DATE, fraud$FIN.TRANSACTION.AMOUNT, ylab = "Transaction in $", 
     xlab = "Time in Months")
#Fine for now, but fix later
plot(fraud.xts, type = "l", ylab = "Transaction in $", xlab = "Time in Months")

hist(fraud.xts)

library(DMwR)
outliers <- lofactor(fraud$FIN.TRANSACTION.AMOUNT)

#To do:
#1) see how to use stl() function and converting finan.trans to ts object
#2) see if the tsoutliers() function better in the forecast package

dput(droplevels(head(fraud$FIN.TRANSACTION.DATE)))
