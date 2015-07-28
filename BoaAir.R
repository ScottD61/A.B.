#Load data
fraud <- read.csv("/Users/scdavis6/Fraud/BoaAir.csv", sep = ",", na.strings = "",
                    header = TRUE)
#Data prep
summary(fraud)
str(fraud)
#Convert to time series objects
#Convert date column from vector to date object
#yes
fraud$FIN.TRANSACTION.DATE <- as.Date(fraud$FIN.TRANSACTION.DATE, "%m/%d/%y")


#check output
head(fraud$FIN.TRANSACTION.DATE, 5)

#Visualizations
#Histogram
hist(fraud$FIN.TRANSACTION.AMOUNT)
#Boxplot
boxplot(fraud$FIN.TRANSACTION.AMOUNT)
boxplot.stats(fraud$FIN.TRANSACTION.AMOUNT)$out
#result has over 3.4k outliers


#To do:
#1) see how to use stl() function and converting finan.trans to ts object
#2) see if the tsoutliers() function better in the forecast package

dput(droplevels(head(fraud$FIN.TRANSACTION.DATE)))
