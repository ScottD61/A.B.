#Load data
fraud <- read.csv("/Users/scdavis6/Fraud/BoaAir.csv", sep = ",", na.strings = "",
                    header = TRUE)

#Data prep
summary(fraud)
str(fraud)
dput(droplevels(head(fraud$FIN.TRANSACTION.DATE)))
#Convert to time series objects
#Convert date column from factor to date object
fraud$FIN.TRANSACTION.DATE <- as.Date(fraud$FIN.TRANSACTION.DATE, "%m/%d/%y")
#Convert numeric and date to xts objects - don't use
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

#ggplot2 way - GOOD 
library(ggplot2)
ggplot(fraud, aes(x = FIN.TRANSACTION.DATE, y = FIN.TRANSACTION.AMOUNT)) + 
    geom_line() + 
    xlab("Transaction Date") + 
    ylab("Transaction Amount")    


#Calculate outliers
library(DMwR)
outliers <- lofactor(fraud$FIN.TRANSACTION.AMOUNT, k = 10)
#Calculate outliers for subsetted data - got NA values - bad
outliers2 <- lofactor(subsetted$FIN.TRANSACTION.AMOUNT, k = 10)


#Go here for ggplot2 graph
#http://www.r-bloggers.com/ryan-peek-on-using-xts-and-ggplot-for-time-series-data/

install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)


#Cluster Analysis 

#todo
#Create proper subset for the two variables
#look at the datatype for the two variables
#change datatype if needed
#Compute distances 
#find optimal value for elip and minpts



#Distance matrix
library(cluster)
#Get required variables
subscan <- c(subsetted$MCH.MERCHANT.NAME, subsetted$FIN.TRANSACTION.AMOUNT)
#Calculate distance matrix
dist <- daisy(subscan, )

library(fpc)
scan <- dbscan(subsetted$FIN.TRANSACTION.AMOUNT, eps = 1, MinPts = 500)
str(scan)
plot(col = scan$cluster + 1L, subsetted$FIN.TRANSACTION.AMOUNT, xlim = c(0,5000), 
     ylim = c(0, 200))

#Evaluate cluster
#Vector of predicted clusters
print.dbscan(scan)
#Noise = 0, cluster = numbers 

