#Index
#Part 1)Loading dataset and variable selection
#Part 2)Clustering comparison with two variables
#Part 3)Clustering comparision with three variables
#Part 4)Export clustering results to .csv files
#Part 5)Look at characteristics of segment level 2
#Part 6)Look at characteristics of segment level 3

#Part 1

#Put all four datasets together
#OOS
OOS <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/OOS.csv", na.strings = "", header = TRUE)
#Modular footage
ModFoot <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/ModFoot.csv", na.strings = "", header = TRUE)
#STR data
STR <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/STR.csv", na.strings = "", header = TRUE)
#PredDay data
PredDay <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/PredDay.csv", na.strings = "", header = TRUE)
#Average deliveries per month
AvgDelMo <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/AvgDelMo.csv", na.strings = "", header = TRUE)
#Merge to create final dataset
SprCentr1 <- merge(OOS, ModFoot, by = "Store.Number")
#Second merge
SprCentr2 <- merge(STR, PredDay, by = "Store.Number")
#Merge two together
SprCentr3 <- merge(SprCentr1, SprCentr2, by = "Store.Number")
#Final merge
SprCentr <- merge(SprCentr3, AvgDelMo, by = "Store.Number")
#Identify row with outlier
a <- SprCentr[SprCentr$Average.Cases.Delivered.Per.Week == 2784,]


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
SprCentr <- SprCentr[-655,]
#Take out missing values
SprCentr <- na.omit(SprCentr)


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


#Reload original dataframe, but do not take out store_Id's
#Put all four datasets together
#OOS
OOS <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/OOS.csv", na.strings = "", header = TRUE)
#Modular footage
ModFoot <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/ModFoot.csv", na.strings = "", header = TRUE)
#STR data
STR <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/STR.csv", na.strings = "", header = TRUE)
#PredDay data
PredDay <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/PredDay.csv", na.strings = "", header = TRUE)
#Average deliveries per month
AvgDelMo <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/AvgDelMo.csv", na.strings = "", header = TRUE)
#Merge to create final dataset
SprCentr1 <- merge(OOS, ModFoot, by = "Store.Number")
#Second merge
SprCentr2 <- merge(STR, PredDay, by = "Store.Number")
#Merge two together
SprCentr3 <- merge(SprCentr1, SprCentr2, by = "Store.Number")
#Final merge
SprCentr <- merge(SprCentr3, AvgDelMo, by = "Store.Number")
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
SprCentr <- SprCentr[-655,]

#Step 2

#Clustering dataset
Wal <- SprCentr[, 3:4]
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



#Results to give
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

#Standardize data
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
Wal3 <- SprCentr[, c(3,4,6)]
daisy8 <- daisy(Wal3, metric = "euclidean", stand = TRUE)
#apply pamk function to find optimal clusters 
system.time(pk2 <- pamk(Wal3, krange = 2:20, usepam = TRUE))
#   user     system elapsed 
#   1319.71  2.64   1337.89 
#2 clusters 
#Apply pam algorithm
answers8 <- pam(daisy8, 2)
#Summarize cluster
answers8$clusinfo

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
#Create 2 dataframes
#Cluster 1 
cl1a <- as.data.frame(x$`1`)
#cluster 2 
cl2a <- as.data.frame(x$`2`)
#Convert to csv
#Export dataset as a whole with both clusters
write.csv(SprCentr, file = "/Users/scdavis6/A.B./Walmart/Walmart/Clustered.csv")
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


#Subset problematic data over 3% OOS Rate and over 400 cases in cluster 1 - use 
#394/1296 30% 
prob1 <- cl1a[which(cl1a$FY.15.OOS.Rate > 0.03 & cl1a$Average.Cases.Delivered.Per.Week > 400),]
#Subset problematic data over 3% OOS Rate and over 400 cases in cluster 2 
#207/1535 14%
prob2 <- cl2a[which(cl2a$FY.15.OOS.Rate > 0.03 & cl2a$Average.Cases.Delivered.Per.Week > 400),]

#Subset stores in cluster 1 over 400 cases delivered
#1020/1296 79% 
prob10 <- cl1a[which(cl1a$Average.Cases.Delivered.Per.Week > 400),]
#Subset stores in cluster 2 over 400 cases delivered 
#288/1535 19% 
prob11 <- cl2a[which(cl2a$Average.Cases.Delivered.Per.Week > 400),]

#Look at size of stores - subset
#519/1296 40%
prob3 <- cl1a[which(cl1a$FY.15.OOS.Rate > 0.03),]
#Subset problematic data over 3% OOS Rate in cluster 2 
prob4 <- cl2a[which(cl2a$FY.15.OOS.Rate > 0.03),]
#1054/1535 69%

#Problematic stores in cluster 1 as csv file 
write.csv(prob1, file = "/Users/scdavis6/A.B./Walmart/Walmart/problematic.csv")


#Get large dataframe minus small dataframe (cl1a - prob1)
#Reload data w/o taking out storeid

#Reload original dataframe, but do not take out store_Id's
#Put all four datasets together
#OOS
OOS <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/OOS.csv", na.strings = "", header = TRUE)
#Modular footage
ModFoot <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/ModFoot.csv", na.strings = "", header = TRUE)
#STR data
STR <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/STR.csv", na.strings = "", header = TRUE)
#PredDay data
PredDay <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/PredDay.csv", na.strings = "", header = TRUE)
#Average deliveries per month
AvgDelMo <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/AvgDelMo.csv", na.strings = "", header = TRUE)
#Merge to create final dataset
SprCentr1 <- merge(OOS, ModFoot, by = "Store.Number")
#Second merge
SprCentr2 <- merge(STR, PredDay, by = "Store.Number")
#Merge two together
SprCentr3 <- merge(SprCentr1, SprCentr2, by = "Store.Number")
#Final merge
SprCentr <- merge(SprCentr3, AvgDelMo, by = "Store.Number")
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
#Take out missing values
SprCentr <- na.omit(SprCentr)
#Took out huge outlier
SprCentr <- SprCentr[-655,]
#Combine cluster result to dataframe
#Export clustering results
#apply cluster ids to original dataframe
SprCentr$cluster <- answers8$cluster
#Get proportions by splitting df by factor
x <- split(SprCentr, SprCentr$cluster)
str(x)
#Create 2 dataframes
#Cluster 1 w/ store id's attached 
cl1a2 <- as.data.frame(x$`1`)
#Create prob1 dataframe  with store id
#Subset problematic data over 3% OOS and 400 cases/wk Rate in cluster 1
prob1a <- cl1a2[which(cl1a2$FY.15.OOS.Rate > 0.03 & cl1a2$Average.Cases.Delivered.Per.Week > 400),]
#Subtract one dataframe from other - Non-problematic stores
prob1ab <- cl1a2[ !(cl1a2$Store.Number %in% prob1a$Store.Number),]
#cl1a2 - prob1a = prob1ab; 1296 - 394 = 902

#Get segment level two information
#Problematic stores
summary(prob1a)
#Non-problematic stores
summary(prob1ab)

#Get problematic stores in region 2 from cluster 1 - don't need 
prob5 <- cl1a2[which(cl1a2$A.B.Region == 2),]

#Percentage under 100 total feet of problematic stores all regions in seg level 2  
#374/1296 29%
probft <- prob1a[which(prob1a$Total.Footage <= 100),]
#Get summary for slides
summary(probft)
#Percentage under 100 total feet of problematic stores in region 2 from cluster 1 - don't need 
probfoot <- prob5[which(prob5$Total.Footage <= 100),]
#Percent under 100 total feet of non-problematic stores all regions in seg level 2
#858/1296 66%
probfootnon <- prob1ab[which(prob1ab$Total.Footage <= 100),]

#Subset prop and nonprob in stores in cluster 1 
#Same OOS Rate and deliveries, but footage between 70-100ft
#non prob called prob1ab
#prob called prob1a
#subset prob - use 
prob1aft <- prob1a[which(prob1a$Total.Footage >= 70 & prob1a$Total.Footage <= 100),]
#summary for segment lvl 3
summary(prob1aft)
#Subset non-prob
#get the nonproblematic region - don't need 
prob1abft <- prob1ab[which(prob1ab$Total.Footage >= 70 & prob1ab$Total.Footage <= 100),]

#Export segment level 3 as .csv file
write.csv(prob1aft, file = "/Users/scdavis6/A.B./Walmart/Walmart/lvl3.csv")


