#Visualizing results with 2 clusters in pam - 3 variables 
#Visualizations of cluster results
#Best one - compares all the plot combintations between three variables 
#PCA plot
#plot results
plot(answers8, data = Wal, main = "")
#Main plot
plot(Wal3, col = answers8$cluster)
legend("topright", legend = paste("clusters", 1:2), pch = 1, col = 1:2)
#Three dimensional scatterplot
library(scatterplot3d)
scatterplot3d(Wal3$FY.15.OOS.Rate, Wal3$Average.Cases.Delivered.Per.Week, Wal3$Total.Footage, 
              col = answers8$cluster)

library(ggplot2)
#Cluster 1 - Low opportunity
#Histograms for each numeric value
#Average deliveries per week - done 
ade <- ggplot(cl1a, aes(x = Average.Deliveries.Per.Week))
adew <- ade + geom_histogram(aes(fill = ..count..))
adew + ggtitle("Barchart of Cluster 1 Average Deliveries Per Week")

#Average deliveries per month - done
adm <- ggplot(cl1a, aes(x = Average.Deliveries.Per.Month))
admw <- adm + geom_histogram(aes(fill = ..count..), binwidth = 1)
admw + ggtitle("Histogram of Cluster 1 Average Deliveries Per Month")

#OOS Rate % - done
os <- ggplot(cl1a, aes(x = FY.15.OOS.Rate))
osr <- os + geom_histogram(aes(fill = ..count..))
osr + ggtitle("Histogram of Cluster 1 OOS Rate %")

#Footage - done
fo <- ggplot(cl1a, aes(x = Total.Footage))
foo <- fo + geom_histogram(aes(fill = ..count..))
foo + ggtitle("Histogram of Cluster 1 Total Footage")

#Avg cases del per wk - done 
vo <- ggplot(cl1a, aes(x = Average.Cases.Delivered.Per.Week))
voo <- vo + geom_histogram(aes(fill = ..count..))
voo + ggtitle("Histogram of Cluster 1 Average Cases Delivered Per Week")


#Apply clustering results to dataframe as separate column - FIX 
Wal$cluster <- answers1$cluster
#Clustering results
acl <- ggplot(Wal, aes(x = FY.15.OOS.Rate, y = Average.Cases.Delivered.Per.Week, color = cluster))
acl + geom_point() + geom_point(cluster, aes(x = ))

#Visualizations per cluster
#Cluster 1
#cluster 1 scatterplot matrix - done 
pairs(~FY.15.OOS.Rate + Total.Footage + Average.Deliveries.Per.Week + 
          Average.Deliveries.Per.Month + Average.Deliveries.Per.Week, data = cl1a, main = 
          "FY 15 Scatterplot Matrix", col = "blue")

#Cluster 1 AB Region histogram - done
ab1 <- ggplot(cl1a, aes(x = A.B.Region))
abc1 <- ab1 + geom_histogram(aes(fill = ..count..)) 
abc1 + ggtitle("Barchart of Cluster 1 A.B. Regions")

#Delivery Schedule - done
ba1 <- ggplot(data = cl1a, aes(factor(Delivery.Schedule))) 
bca1 <- ba1 + geom_bar(aes(fill = ..count..))  
bcad1 <- bca1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
bcad1 + ggtitle("Barchart of Cluster 1 Delivery Schedule Days")

#AB Region compared to OOS Rate - done 
c1 <- ggplot(cl1a, aes(A.B.Region, FY.15.OOS.Rate))
c1 + geom_boxplot(aes(color = factor(A.B.Region))) + ggtitle("Cluster 1 A.B.Region vs OOS Rate %")

#Delivery schedule compared to OOS Rate - fix 
da <- ggplot(cl1a, aes(factor(Delivery.Schedule), FY.15.OOS.Rate))
dba <- da + geom_boxplot(aes(color = Delivery.Schedule)) 
dbac <- dba + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dbac + ggtitle("Cluster 1 Delivery Schedule vs OOS Rate %")


#Cluster 2 - Highest Opportunity 
#Average deliveries per week - done - 1 
ade <- ggplot(cl2a, aes(x = Average.Deliveries.Per.Week))
adew <- ade + geom_histogram(aes(fill = ..count..))
adew + ggtitle("Barchart of Cluster 2 Average Deliveries Per Week")

#Average deliveries per month - done
adm <- ggplot(cl2a, aes(x = Average.Deliveries.Per.Month))
admw <- adm + geom_histogram(aes(fill = ..count..), binwidth = 1)
admw + ggtitle("Histogram of Cluster 2 Average Deliveries Per Month")

#OOS Rate % - done
os <- ggplot(cl2a, aes(x = FY.15.OOS.Rate))
osr <- os + geom_histogram(aes(fill = ..count..))
osr + ggtitle("Histogram of Cluster 2 OOS Rate %")

#Footage - done
fo <- ggplot(cl2a, aes(x = Total.Footage))
foo <- fo + geom_histogram(aes(fill = ..count..))
foo + ggtitle("Histogram of Cluster 2 Total Footage")

#Avg cases del per wk - done 
vo2 <- ggplot(cl2a, aes(x = Average.Cases.Delivered.Per.Week))
voo2 <- vo2 + geom_histogram(aes(fill = ..count..))
voo2 + ggtitle("Histogram of Cluster 2 Average Cases Delivered Per Week")

#cluster 2 scatterplot matrix - done 
pairs(~FY.15.OOS.Rate + Total.Footage + Average.Deliveries.Per.Week + 
          Average.Deliveries.Per.Month + Average.Deliveries.Per.Week, data = cl2a, main = 
          "FY 15 Scatterplot Matrix", col = "blue")

#Cluster 2 AB Region histogram - done
ab2 <- ggplot(cl2a, aes(x = A.B.Region))
abc2 <- ab2 + geom_histogram(aes(fill = ..count..)) 
abc2 + ggtitle("Barchart of Cluster 2 A.B. Regions")

#Delivery Schedule - done
ba2 <- ggplot(data = cl2a, aes(factor(Delivery.Schedule))) 
bca2 <- ba2 + geom_bar(aes(fill = ..count..))  
bcad2 <- bca2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
bcad2 + ggtitle("Barchart of Cluster 2 Delivery Schedule Days")

#AB Region compared to OOS Rate - done 
c2 <- ggplot(cl2a, aes(A.B.Region, FY.15.OOS.Rate))
c2 + geom_boxplot(aes(color = factor(A.B.Region))) + ggtitle("Cluster 2 A.B.Region vs OOS Rate %")

#Delivery schedule compared to OOS Rate - fix 
da2 <- ggplot(cl2a, aes(factor(Delivery.Schedule), FY.15.OOS.Rate))
dba2 <- da2 + geom_boxplot(aes(color = Delivery.Schedule)) 
dbac2 <- dba2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dbac2 + ggtitle("Cluster 2 Delivery Schedule vs OOS Rate %")





#Graphics of problematic regions in Cluster 1
#Cluster 1 - Low opportunity
#Histograms for each numeric value
#Average deliveries per week - done 
adep <- ggplot(prob1, aes(x = Average.Deliveries.Per.Week))
adewp <- adep + geom_histogram(aes(fill = ..count..))
adewp + ggtitle("Histogram of Cluster 1 Average Deliveries Per Week")

#Average deliveries per month - done
admp <- ggplot(prob1, aes(x = Average.Deliveries.Per.Month))
admwp <- admp + geom_histogram(aes(fill = ..count..), binwidth = 1)
admwp + ggtitle("Histogram of Cluster 1 Average Deliveries Per Month")

#OOS Rate % - done
osp <- ggplot(prob1, aes(x = FY.15.OOS.Rate))
osrp <- osp + geom_histogram(aes(fill = ..count..))
osrp + ggtitle("Histogram of Cluster 1 Average Deliveries Per Month")

#Footage - done
fop <- ggplot(prob1, aes(x = Total.Footage))
foop <- fop + geom_histogram(aes(fill = ..count..))
foop + ggtitle("Histogram of Cluster 1 Total Footage")

#Avg cases del per wk - done 
vop <- ggplot(prob1, aes(x = Average.Cases.Delivered.Per.Week))
voop <- vop + geom_histogram(aes(fill = ..count..))
voop + ggtitle("Histogram of Cluster 1 Average Cases Delivered Per Week")


#Apply clustering results to dataframe as separate column - FIX 
Wal$cluster <- answers1$cluster
#Clustering results
aclp <- ggplot(Wal, aes(x = FY.15.OOS.Rate, y = Average.Cases.Delivered.Per.Week, color = cluster))
aclp + geom_point() + geom_point(cluster, aes(x = ))

#Visualizations per cluster
#Cluster 1
#cluster 1 scatterplot matrix - done 
pairs(~FY.15.OOS.Rate + Total.Footage + Average.Deliveries.Per.Week + 
          Average.Deliveries.Per.Month + Average.Deliveries.Per.Week, data = prob1, main = 
          "FY 15 Scatterplot Matrix", col = "blue")

#Cluster 1 AB Region histogram - done
ab1p <- ggplot(prob1, aes(x = A.B.Region))
abc1p <- ab1p + geom_histogram(aes(fill = ..count..)) 
abc1p + ggtitle("Barchart of Cluster 1 A.B. Regions")

#Delivery Schedule - done
ba1p <- ggplot(data = prob1, aes(factor(Delivery.Schedule))) 
bca1p <- ba1p + geom_bar(aes(fill = ..count..))  
bcad1p <- bca1p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
bcad1p + ggtitle("Barchart of Cluster 1 Delivery Schedule Days")

#AB Region compared to OOS Rate - done 
c1p <- ggplot(prob1, aes(A.B.Region, FY.15.OOS.Rate))
c1p + geom_boxplot(aes(color = factor(A.B.Region))) + ggtitle("Cluster 1 A.B.Region vs OOS Rate %")

#Delivery schedule compared to OOS Rate - fix 
dap <- ggplot(prob1, aes(factor(Delivery.Schedule), FY.15.OOS.Rate))
dbap <- dap + geom_boxplot(aes(color = Delivery.Schedule)) 
dbacp <- dbap + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dbacp + ggtitle("Cluster 1 Delivery Schedule vs OOS Rate %")



#Problematic stores in cluster 2
#Average deliveries per week - done - 1 
ade <- ggplot(prob2, aes(x = Average.Deliveries.Per.Week))
adew <- ade + geom_histogram(aes(fill = ..count..))
adew + ggtitle("Histogram of Cluster 2 Average Deliveries Per Week")

#Average deliveries per month - done
adm <- ggplot(prob2, aes(x = Average.Deliveries.Per.Month))
admw <- adm + geom_histogram(aes(fill = ..count..), binwidth = 1)
admw + ggtitle("Histogram of Cluster 2 Average Deliveries Per Month")

#OOS Rate % - done
os <- ggplot(prob2, aes(x = FY.15.OOS.Rate))
osr <- os + geom_histogram(aes(fill = ..count..))
osr + ggtitle("Histogram of Cluster 2 OOS Rate %")

#Footage - done
fo <- ggplot(prob2, aes(x = Total.Footage))
foo <- fo + geom_histogram(aes(fill = ..count..))
foo + ggtitle("Histogram of Cluster 2 Total Footage")

#Avg cases del per wk - done 
vo2 <- ggplot(prob2, aes(x = Average.Cases.Delivered.Per.Week))
voo2 <- vo2 + geom_histogram(aes(fill = ..count..))
voo2 + ggtitle("Histogram of Cluster 2 Average Cases Delivered Per Week")

#cluster 2 scatterplot matrix - done 
pairs(~FY.15.OOS.Rate + Total.Footage + Average.Deliveries.Per.Week + 
          Average.Deliveries.Per.Month + Average.Deliveries.Per.Week, data = prob2, main = 
          "FY 15 Scatterplot Matrix", col = "blue")

#Cluster 2 AB Region histogram - done
ab2 <- ggplot(prob2, aes(x = A.B.Region))
abc2 <- ab2 + geom_histogram(aes(fill = ..count..)) 
abc2 + ggtitle("Barchart of Cluster 2 A.B. Regions")

#Delivery Schedule - done
ba2 <- ggplot(data = prob2, aes(factor(Delivery.Schedule))) 
bca2 <- ba2 + geom_bar(aes(fill = ..count..))  
bcad2 <- bca2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
bcad2 + ggtitle("Barchart of Cluster 2 Delivery Schedule Days")

#AB Region compared to OOS Rate - done 
c2 <- ggplot(prob2, aes(A.B.Region, FY.15.OOS.Rate))
c2 + geom_boxplot(aes(color = factor(A.B.Region))) + ggtitle("Cluster 2 A.B.Region vs OOS Rate %")

#Delivery schedule compared to OOS Rate - fix 
da2 <- ggplot(prob2, aes(factor(Delivery.Schedule), FY.15.OOS.Rate))
dba2 <- da2 + geom_boxplot(aes(color = Delivery.Schedule)) 
dbac2 <- dba2 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dbac2 + ggtitle("Cluster 2 Delivery Schedule vs OOS Rate %")