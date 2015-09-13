#Load .csv file
SprCentr <- read.csv("/Users/scdavis6/Fraud/Walmart/Walmart/Final.csv", na.strings = "", 
                     header = TRUE)
#Analyze data
summary(SprCentr)
str(SprCentr)

#Cleanup
#Convert store number to factor
SprCentr$Store.Number <- as.factor(SprCentr$Store.Number)
#Convert average del per week to numeric
SprCentr$Average.Deliveries.Per.Week <- as.factor(SprCentr$Average.Deliveries.Per.Week)
#Convert average del per month to numeric
SprCentr$Average.Deliveries.Per.Month <- as.factor(SprCentr$Average.Deliveries.Per.Month)
#Convert average cases del per week to numeric
SprCentr$Average.Cases.Delivered.Per.Week <- as.numeric(SprCentr$Average.Cases.Delivered.Per.Week)
#Convert OOS Rate to numeric
SprCentr$FY.15.OOS.Rate <- as.numeric(sub("%", "", SprCentr$FY.15.OOS.Rate))/100
#Convert Region to factor
SprCentr$A.B.Region <- as.factor(SprCentr$A.B.Region)
#Take out storetype column
SprCentr$Store.Type <- NULL
#Take out storenumber column
SprCentr$Store.Number <- NULL
#Took out huge outlier
SprCentr <- SprCentr[-657,]

#Impute missing values using kNN


#Take out missing values
SprCentr <- na.omit(SprCentr)


#Significance tests for independence
#Chi-sq
#Independence between OOS rate and avg del per week
chisq.test(SprCentr$FY.15.OOS.Rate, SprCentr$Average.Deliveries.Per.Week)
#Independence between OOS rate and avg del per month
chisq.test(SprCentr$FY.15.OOS.Rate, SprCentr$Average.Deliveries.Per.Month)

#Do correlation instead
#Between numeric variables
library(Hmisc)
#Between OOS rate and avg del per week
a <- cor.test(SprCentr$FY.15.OOS.Rate, SprCentr$Average.Deliveries.Per.Week, 
              method = "pearson")
print(a)
#Between OOS rate and avg del per month
b <- cor.test(SprCentr$FY.15.OOS.Rate, SprCentr$Average.Deliveries.Per.Month, 
              method = "pearson")
print(b)
#Between OOS rate and average cases delivered per week
c <- cor.test(SprCentr$FY.15.OOS.Rate, SprCentr$Average.Cases.Delivered.Per.Week, 
              method = "pearson")
print(c)
#Between OOS rate and total footage of store
d <- cor.test(SprCentr$FY.15.OOS.Rate, SprCentr$Total.Footage, 
              method = "pearson")
print(d)


#Strength of association 
#Mixed variables
#Between OOS rate and region 
Model1 <- lm(FY.15.OOS.Rate ~ A.B.Region, SprCentr)
summary(Model1)
#Get sample multiple correlation coefficient, R
rsq1 <- summary(Model1)$r.squared
rsq1 #low relationship b/c near zero AND signifcant on only SOME factors

#Between OOS rate and delivery schedule
Model2 <- lm(FY.15.OOS.Rate ~ Delivery.Schedule, SprCentr)
summary(Model2)
#Get sample multiple correlation coefficient, R
rsq2 <- summary(Model2)$r.squared
rsq2 #low relationship b/c near zero AND significant on only SOME factors

#Variable selection
#Correlation between independent variables
#Avg del per week and avg del per month
cor(SprCentr$Average.Deliveries.Per.Week, SprCentr$Average.Deliveries.Per.Month) #very high
#Avg del per week and avg cases del per week
cor(SprCentr$Average.Deliveries.Per.Week, SprCentr$Average.Cases.Delivered.Per.Week) #hardly
#Avg case del per week and total footage
cor(SprCentr$Total.Footage, SprCentr$Average.Cases.Delivered.Per.Week) #some
#Avg del per month and total footage
cor(SprCentr$Total.Footage, SprCentr$Average.Deliveries.Per.Month) #not really
#Avg del per month and avg case del per week
cor(SprCentr$Average.Cases.Delivered.Per.Week, SprCentr$Average.Deliveries.Per.Month) #some

#Inference - linear regression
#All variables
Model3 <- lm(FY.15.OOS.Rate ~., data = SprCentr)
summary(Model3)
#Excluding avg del per week
Model4 <- lm(FY.15.OOS.Rate ~ Average.Deliveries.Per.Month + Total.Footage +  
                 Average.Cases.Delivered.Per.Week + A.B.Region + Delivery.Schedule, data = SprCentr)
summary(Model4)

#Evaluate model
#Identify non-linearity with residual plot
#Get residuals
resid(Model4)
#Density plot of residuals
plot(density(resid(Model4)))
qqnorm(resid(Model4))
qqline(resid(Model4))

plot(Model4)
#Residuals vs fitted - mostly clustered at 0 constant variance - homosckedastic
#Normal Q-Q - mostly right skew 
#Residuals vs leverage -  help determines if few obs driving regr

#Results - look at intro stat learning pg 108 for list of things
#Linear: slightly NO from res vs fitted b/c the red line is not completely straight
#Autocorrelation: not time series (dont have to test)
#Heteroskedastic: NO from res vs fitted
#Outliers: YES from res vs fitted, but does not influence fit
#Leverage points: high leverage, high residual - BAD 
#Multicollinearity: removed avg del per wk, now none

#Test for multicollinearity in both models
library(car)
multi1 <- vif(Model3)
multi1
multi2 <- vif(Model4)
multi2
#multicollinearity high between wk and mo, so removed wk





#Plot residuals against all variables =- WRONG ONLY DIAGNOSE HETER OF EACH VARIABLES
#WHEN NOT MULTIPLE REGRESSION 
#Residuals vs avg del per month
res <- resid(Model4)
plot(SprCentr$Average.Deliveries.Per.Month, res)
abline(0, 0)
#Residuals vs avg cases del per week
plot(SprCentr$Average.Cases.Delivered.Per.Week, res)
abline(0,0)
#Residuals vs total footage
plot(SprCentr$Total.Footage, res)
abline(0,0)


#updated dataset
#Load .csv file
SprCentr2 <- read.csv("\\\\na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/Final2.csv",
                      na.strings = "", header = TRUE)
#Feature engineering
#Convert store number to factor
SprCentr2$Store.Number <- as.factor(SprCentr2$Store.Number)
#Convert average del per week to numeric
SprCentr2$Average.Deliveries.Per.Week <- as.factor(SprCentr2$Average.Deliveries.Per.Week)
#Convert average del per month to numeric
SprCentr2$Average.Deliveries.Per.Month <- as.factor(SprCentr2$Average.Deliveries.Per.Month)
#Convert average cases del per week to numeric
SprCentr2$Average.Cases.Delivered.Per.Week <- as.numeric(SprCentr2$Average.Cases.Delivered.Per.Week)
#Convert OOS Rate to numeric
SprCentr2$FY.15.OOS.Rate <- as.numeric(sub("%", "", SprCentr2$FY.15.OOS.Rate))/100
#Convert Region to factor
SprCentr2$A.B.Region <- as.factor(SprCentr2$A.B.Region)
#Convert avg day of supply to numeric
SprCentr2$Avg.Day.of.Supply <- as.numeric(SprCentr2$Avg.Day.of.Supply)
#Take out storetype column
SprCentr2$Store.Type <- NULL
#Take out storenumber column
SprCentr2$Store.Number <- NULL
#Take out missing values
SprCentr2 <- na.omit(SprCentr2)


#Correlation 
e <- cor.test(SprCentr2$FY.15.OOS.Rate, SprCentr2$Avg.Day.of.Supply, 
              method = "pearson")
print(e)

#Visualizations
#scatterplot matrix
pairs(~FY.15.OOS.Rate + Total.Footage + Average.Deliveries.Per.Week + 
          Average.Deliveries.Per.Month + Average.Deliveries.Per.Week + Avg.Day.of.Supply,
      data = SprCentr2, main = "FY 15 Scatterplot Matrix")

#Inference
#Excluding avg del per week
Model5 <- lm(FY.15.OOS.Rate ~ Average.Deliveries.Per.Month + Total.Footage +  
                 Average.Cases.Delivered.Per.Week + A.B.Region + Delivery.Schedule + 
                 Avg.Day.of.Supply, data = SprCentr2)
summary(Model5)

#Evaluate model
#Identify non-linearity with residual plot
#Get residuals
resid(Model5)
#Density plot of residuals
plot(density(resid(Model5)))
qqnorm(resid(Model5))
qqline(resid(Model5))

plot(Model5)
#Residuals vs fitted - mostly clustered at 0 constant variance - homosckedastic
#Normal Q-Q - mostly right skew 
#Residuals vs leverage -  help determines if few obs driving regr

#Results - look at intro stat learning pg 108 for list of things
#Linear: slightly YES from res vs fitted b/c the red line is not completely straight
#Autocorrelation: not time series (dont have to test)
#Heteroskedastic: NO from res vs fitted, they are clustered
#Outliers: YES from res vs fitted, but does not influence fit
#Leverage points: low leverage, high residual - BAD 
#Multicollinearity: removed avg del per wk, now none

#Test for multicollinearity with avg day of supply
library(car)
multi3 <- vif(Model5)
multi3
#multicollinearity not high for any


#test for greater R^2
Model6 <- lm(FY.15.OOS.Rate ~ Average.Deliveries.Per.Week + Total.Footage +  
                 Average.Cases.Delivered.Per.Week + A.B.Region + Delivery.Schedule + 
                 Avg.Day.of.Supply, data = SprCentr2)
summary(Model6)
# R^2 lower with avg del per week instead of avg del per month




#Clustering
#Clustering dataset
Wal <- SprCentr[, 3:4]
#Scale dataset
#Implement OPTICS algorithm - don't use too hard
library(dbscan)
res <- optics(Wal, eps = 10, minPts = 35, eps_cl = 10)
#Plot findings
plot(res)
plot(Wal, res$cluster)
#Main plot to see evaluation 
plot(answers1, data = Wal, main = )


#"Fix" dataset
SprCentr <- SprCentr[-660,]

#Clustered with two variables 
library(cluster)
#Create dissimilarity matrix
#Get system time
system.time(daisy1 <- daisy(Wal, metric = "euclidean"))
#Pam algorithm with 3 clusters
#Get system time
system.time(answers1 <- pam(daisy1, 3, diss = TRUE))
#Clustering results
#Information about clusters
answers1$clusinfo
#Number of observations per cluster
answers1$id.med
#Medoid ids
answers1$medoids
#Plot medoids - main plot to see evaluation 
plot(answers1, data = Wal, main = "")
#Best one 
plot(Wal, col = answers1$cluster)
legend("topright", legend = paste("clusters", 1:3), pch = 1, col = 1:3)

#Reload original dataframe, but do not take out store_Id's
#Load .csv file
SprCentr <- read.csv("\\\\na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/Final.csv",
                     na.strings = "", header = TRUE)
#Cleanup
#Convert store number to factor
SprCentr$Store.Number <- as.factor(SprCentr$Store.Number)
#Convert average del per week to numeric
SprCentr$Average.Deliveries.Per.Week <- as.numeric(SprCentr$Average.Deliveries.Per.Week)
#Convert average del per month to numeric
SprCentr$Average.Deliveries.Per.Month <- as.numeric(SprCentr$Average.Deliveries.Per.Month)
#Convert average cases del per week to numeric
SprCentr$Average.Cases.Delivered.Per.Week <- as.numeric(SprCentr$Average.Cases.Delivered.Per.Week)
#Convert OOS Rate to numeric
SprCentr$FY.15.OOS.Rate <- as.numeric(sub("%", "", SprCentr$FY.15.OOS.Rate))/100
#Convert Region to factor
SprCentr$A.B.Region <- as.factor(SprCentr$A.B.Region)
#Take out storetype column
SprCentr$Store.Type <- NULL
#Take out missing values
SprCentr <- na.omit(SprCentr)
#Took out huge outlier
SprCentr <- SprCentr[-657,]

#Export results
#apply cluster ids to original dataframe
SprCentr$cluster <- answers1$cluster
#Get proportions by splitting df by factor
x <- split(SprCentr, SprCentr$cluster)
str(x)
#Create 3 dataframes
#Cluster 1 - medium priority
cl1 <- as.data.frame(x$`1`)
#cluster 2 - low priority
cl2 <- as.data.frame(x$`2`)
#cluster 3 - high priority
cl3 <- as.data.frame(x$`3`)
#Convert to csv
#cluster1
write.csv(cl1, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/cluster1.csv")
#cluster2
write.csv(cl2, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/cluster2.csv")
#cluster3
write.csv(cl3, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/cluster3.csv")

#Demographics of each cluster
#Cluster 1 - medium priority
summary(cl1)
#Find mode of cluster 1

#Cluster 2 - low priority
summary(cl2)

#Cluster 3 - highpriority
summary(cl3)

plot(SprCentr$Average.Cases.Delivered.Per.Week, SprCentr$FY.15.OOS.Rate)

#estimate number of clusters w/ pamk function in fpc package
#Based on average silhouette size
library(fpc)
#apply pamk function
system.time(pk1 <- pamk(Wal, krange = 2:20, usepam = TRUE))
# user     system elapsed 
# 1592.29  1.10   1602.05 
#2 clusters


#Cluster evaluation
#Compare pam to k-means 
#Find optimal number of clusters for kmeans 
#Standardize data
Wal2 <- scale(Wal, scale = TRUE)
wss <- (nrow(Wal2)-1)*sum(apply(Wal2, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(Wal2, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
#9 clusters
system.time(answersk1 <- kmeans(Wal, 3))

#Compare k-means to PAM
#kmedoids
comp1 <- cluster.stats(d = daisy1, answers1$cluster)
#kmeans
comp2 <- cluster.stats(d = daisy1, answersk1$cluster)



#With scaled dataset
Wal1 <- scale(Wal)
library(cluster)
#Create dissimilarity matrix
#Get system time
system.time(daisy3 <- daisy(Wal1, metric = "euclidean"))
#Pam algorithm with 3 clusters
#Get system time
system.time(answers3 <- pam(daisy3, 3, diss = TRUE))
#Best plot
plot(Wal1, col = answers3$cluster)
legend("topright", legend = paste("clusters", 1:3), pch = 1, col = 1:3)
#Plot 2
plot(answers3, data = Wal1, main = "")
#cluster evaluation
comp3 <- cluster.stats(d = daisy3, answers3$cluster)
comp3

#pam w/ standardized in dissimilarity matrix - terrible performance
daisy6 <- daisy(Wal, metric = "euclidean", stand = TRUE )
answers4 <- pam(daisy6, 3)
#plot results
plot(answers4, data = Wal, main = "")
#another plot
plot(Wal, col = answers4$cluster)
#Evaluate
comp6 <- cluster.stats(d = daisy6, answers4$cluster)
comp6

#pam w/ standardized plus total footage
Wal1 <- SprCentr[, 3:5]
daisy5 <- daisy(Wal1, metric = "euclidean", stand = TRUE)
answers5 <- pam(daisy5, 3)
#Evaluate
comp5 <- cluster.stats(d = daisy5, answers5$cluster)
comp5

#create different datasets
SprCentr1 <- read.csv("\\\\na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/Final.csv",
                      na.strings = "", header = TRUE)
#Cleanup
#Convert store number to factor
SprCentr1$Store.Number <- as.factor(SprCentr1$Store.Number)
#Convert average del per week to numeric
SprCentr1$Average.Deliveries.Per.Week <- as.factor(SprCentr1$Average.Deliveries.Per.Week)
#Convert average del per month to numeric
SprCentr1$Average.Deliveries.Per.Month <- as.factor(SprCentr1$Average.Deliveries.Per.Month)
#Convert average cases del per week to numeric
SprCentr1$Average.Cases.Delivered.Per.Week <- as.numeric(SprCent1r$Average.Cases.Delivered.Per.Week)
#Convert OOS Rate to numeric
SprCentr1$FY.15.OOS.Rate <- as.numeric(sub("%", "", SprCentr1$FY.15.OOS.Rate))/100
#Convert Region to factor
SprCentr1$A.B.Region <- as.factor(SprCentr1$A.B.Region)
#Take out storetype column
SprCentr1$Store.Type <- NULL
#Take out storenumber column
SprCentr1$Store.Number <- NULL
#Took out huge outlier
SprCentr1 <- SprCentr1[-657,]

#Impute missing values using kNN

#Take out missing values
SprCentr1 <- na.omit(SprCentr1)

#Attach to original
SprCentr1$cluster <- answers5$cluster
#Get proportions by splitting df by factor
xy <- split(SprCentr1, SprCentr1$cluster)
str(xy)
#Create 3 dataframes
#Cluster 1 - medium priority
cl1y <- as.data.frame(xy$`1`)
#cluster 2 - low priority
cl2y <- as.data.frame(xy$`2`)
#cluster 3 - high priority
cl3y <- as.data.frame(xy$`3`)
#Convert to csv
#cluster1
write.csv(cl1y, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/cluster1y.csv")
#cluster2
write.csv(cl2y, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/cluster2y.csv")
#cluster3
write.csv(cl3y, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/cluster3y.csv")

#plot results


#Results to give - finished version 
#pam w/ standardized dissimilarity matrix 
library(cluster)
daisy7 <- daisy(Wal, metric = "euclidean", stand = TRUE)
#Pam
answers7 <- pam(daisy7, 2)
#plot results
plot(answers7, data = Wal, main = "")
#Another plot
plot(Wal, col = answers7$cluster, main = "Clustering Results")
legend("topright", legend = paste("clusters", 1:2), pch = 1, col = 1:2)

#Standardize data for kmeans 
Wal2 <- scale(Wal, scale = TRUE)
wss <- (nrow(Wal2)-1)*sum(apply(Wal2, 2, var))
for (i in 2:15) wss[i] <- sum(kmeans(Wal2, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")


#Evaluate
library(fpc)
#Pam
comp7 <- cluster.stats(d = daisy7, answers7$cluster)
comp7
#kmeans 
system.time(answersk1 <- kmeans(Wal2, 9))
comp8 <- cluster.stats(d = daisy7, answersk1$cluster)
comp8

#New trial - USE THIS FOR FINAL STUDY 
#Add footage to clustering results
Wal3 <- SprCentr[, 3:5]
daisy8 <- daisy(Wal3, metric = "euclidean", stand = TRUE)
#apply pamk function to find optimal clusters 
system.time(pk2 <- pamk(Wal3, krange = 2:20, usepam = TRUE))
#   user     system elapsed 
#   1319.71  2.64   1337.89 
#2 clusters 
#Apply pam algorithm
answers8 <- pam(daisy8, 2)

#Standardize data
Wal4 <- scale(Wal3, scale = TRUE)
#Find number of clusters minimizing SSE for kmeans 
wss2 <- (nrow(Wal4)-1)*sum(apply(Wal4, 2, var))
for (i in 2:15) wss2[i] <- sum(kmeans(Wal4, centers = i)$withinss)
#Plot results
plot(1:15, wss2, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")
#number of clusters: 12
#Apply kmeans
system.time(answersk2 <- kmeans(Wal4, 12))

#Evaluate both with three variables 
#Pam - USE THIS 
comp9 <- cluster.stats(d = daisy8, answers8$cluster)
comp9
#kmeans
comp10 <- cluster.stats(d = daisy8, answersk2$cluster)
comp10

#Evaluate pam with 2 and pam with 3 variables
#Pam with 2
comp7
#Pam with 3
comp9

#Export clustering results
#apply cluster ids to original dataframe
SprCentr$cluster <- answers8$cluster
#Get proportions by splitting df by factor
x <- split(SprCentr, SprCentr$cluster)
str(x)
#Create 3 dataframes
#Cluster 1 
cl1a <- as.data.frame(x$`1`)
#cluster 2 
cl2a <- as.data.frame(x$`2`)
#Convert to csv
#cluster1
write.csv(cl1, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/cluster1a.csv")
#cluster2
write.csv(cl2, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/cluster2a.csv")

#Demographics of each cluster
#Cluster 1 - medium priority
summary(cl1a)
#Find mode of cluster 1

#Cluster 2 - low priority
summary(cl2a)


#Subset problematic data over 3% OOS Rate in cluster 1
prob1 <- cl1a[which(cl1a$FY.15.OOS.Rate > 0.03 & cl1a$Average.Cases.Delivered.Per.Week > 400),]
#Subset problematic data over 3% OOS Rate in cluster 2 
prob2 <- cl2a[which(cl2a$FY.15.OOS.Rate > 0.03 & cl2a$Average.Cases.Delivered.Per.Week > 400),]

#Look at size of stores - subset
prob3 <- cl1a[which(cl1a$FY.15.OOS.Rate > 0.03),]
#Subset problematic data over 3% OOS Rate in cluster 2 
prob4 <- cl2a[which(cl2a$FY.15.OOS.Rate > 0.03),]

