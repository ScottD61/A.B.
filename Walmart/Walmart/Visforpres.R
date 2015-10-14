#Visualizations for presentation
#Load data for all walmart data 
Allstores <- read.csv("\\\\na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/AllData.csv",
                     na.strings = "", header = TRUE)
#Cluster 1 data
cl1a <- as.data.frame(x$`1`)
#Load library ggplot2
library(ggplot2)

#Vis1
#re-order levels for store.type - doesn't work 
Allstores$Store.Typelevels <- factor(Allstores$Store.Type, levels = c("BASE STR SUPERCENTER", "BASE STR NGHBRHD MKT", "BASE STR WAL-MART"))
#Barplot of stores by store type - done 
all <- ggplot(Allstores, aes(x = Store.Type))
alls <- all + geom_bar(aes(fill = ..count..)) 
alls + ggtitle("Barchart of Stores by Storetype")

#Vis2
#barplot of stores by region - done
alr <- ggplot(Allstores, aes(x = A.B.Region))
alrs <- alr + geom_bar(aes(fill = ..count..)) 
alrsa <- alrs + scale_x_continuous(breaks = round(seq(min(Allstores$A.B.Region), max(Allstores$A.B.Region), by = 1),1))
alrsa + ggtitle("Barchart of All Walmart Stores by Region")

#Vis3
#Load data for bleeding stores 
bleeding <- read.csv("\\\\na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/bleeding.csv",
                      na.strings = "", header = TRUE)

#Barplot of bleeding stores by store type - fix
all <- ggplot(bleeding, aes(x = Region, y = Volume.Delivered))
alls <- all + geom_bar(stat = "identity") 
allse <- alls + facet_grid(. ~ Store.Type) 
allser <- allse + scale_x_continuous(breaks = round(seq(min(bleeding$Region), max(bleeding$Region), by = 1),1))
allsera <- allser + scale_y_continuous(bleeding$Volume.Delivered)
allsera + ggtitle("Barchart of Total Cases Delivered 1/14-7/14 FY14 vs 1/15-7/15")

#Vis4
#Load original OOS data
#Merge to Allstores data
#Get average of FY15 vs FY14 difference for each chain and region occurence 
OOS <- read.csv("\\\\na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/AvgOOS.csv",
                na.strings = "", header = TRUE)
#Remove duplicate stores
OOS <- OOS[!duplicated(OOS$Store.Number), ]
#Merge stores by store.id
Avgoos <- merge(OOS, Allstores, by = "Store.Number")
#Get average of FY15 vs FY14 difference for each chain and region occurence 
#Get all subsets for region 1
#Supercenter 
rg1Spr <- Avgoos[which(Avgoos$Store.Type == "BASE STR SUPERCENTER" & Avgoos$A.B.Region == 1),]
res1 <- mean(rg1Spr$FY.15.vs.FY.14.Change)
#Base Walmart 
rg1Base <- Avgoos[which(Avgoos$Store.Type == "BASE STR WAL-MART" & Avgoos$A.B.Region == 1),]
res2 <- mean(rg1Base$FY.15.vs.FY.14.Change)
#Neighborhood Mkt
rg1Neig <- Avgoos[which(Avgoos$Store.Type == "BASE STR NGHBRHD MKT" & Avgoos$A.B.Region == 1),]
res3 <- mean(rg1Neig$FY.15.vs.FY.14.Change)

#Get all subsets for region 2
#Supercenter 
rg2Spr <- Avgoos[which(Avgoos$Store.Type == "BASE STR SUPERCENTER" & Avgoos$A.B.Region == 2),]
res4 <- mean(rg2Spr$FY.15.vs.FY.14.Change)
#Base Walmart 
rg2Base <- Avgoos[which(Avgoos$Store.Type == "BASE STR WAL-MART" & Avgoos$A.B.Region == 2),]
res5 <- mean(rg2Base$FY.15.vs.FY.14.Change)
#Neighborhood Mkt
rg2Neig <- Avgoos[which(Avgoos$Store.Type == "BASE STR NGHBRHD MKT" & Avgoos$A.B.Region == 2),]
res6 <- mean(rg2Neig$FY.15.vs.FY.14.Change)

#Get all subsets for region 3
#Supercenter 
rg3Spr <- Avgoos[which(Avgoos$Store.Type == "BASE STR SUPERCENTER" & Avgoos$A.B.Region == 3),]
res7 <- mean(rg3Spr$FY.15.vs.FY.14.Change)
#Base Walmart 
rg3Base <- Avgoos[which(Avgoos$Store.Type == "BASE STR WAL-MART" & Avgoos$A.B.Region == 3),]
res8 <- mean(rg3Base$FY.15.vs.FY.14.Change)
#Neighborhood Mkt
rg3Neig <- Avgoos[which(Avgoos$Store.Type == "BASE STR NGHBRHD MKT" & Avgoos$A.B.Region == 3),]
res9 <- mean(rg3Neig$FY.15.vs.FY.14.Change)

#Get all subsets for region 4
#Supercenter 
rg4Spr <- Avgoos[which(Avgoos$Store.Type == "BASE STR SUPERCENTER" & Avgoos$A.B.Region == 4),]
res10 <- mean(rg4Spr$FY.15.vs.FY.14.Change)
#Base Walmart 
rg4Base <- Avgoos[which(Avgoos$Store.Type == "BASE STR WAL-MART" & Avgoos$A.B.Region == 4),]
res11 <- mean(rg4Base$FY.15.vs.FY.14.Change)
#Neighborhood Mkt
rg4Neig <- Avgoos[which(Avgoos$Store.Type == "BASE STR NGHBRHD MKT" & Avgoos$A.B.Region == 4),]
res12 <- mean(rg4Neig$FY.15.vs.FY.14.Change)

#Get all subsets for region 5
#Supercenter 
rg5Spr <- Avgoos[which(Avgoos$Store.Type == "BASE STR SUPERCENTER" & Avgoos$A.B.Region == 5),]
res13 <- mean(rg5Spr$FY.15.vs.FY.14.Change)
#Base Walmart 
rg5Base <- Avgoos[which(Avgoos$Store.Type == "BASE STR WAL-MART" & Avgoos$A.B.Region == 5),]
res14 <- mean(rg5Base$FY.15.vs.FY.14.Change)
#Neighborhood Mkt
rg5Neig <- Avgoos[which(Avgoos$Store.Type == "BASE STR NGHBRHD MKT" & Avgoos$A.B.Region == 5),]
res15 <- mean(rg5Neig$FY.15.vs.FY.14.Change)

#Get all subsets for region 6
#Supercenter 
rg6Spr <- Avgoos[which(Avgoos$Store.Type == "BASE STR SUPERCENTER" & Avgoos$A.B.Region == 6),]
res16 <- mean(rg6Spr$FY.15.vs.FY.14.Change)
#Base Walmart 
rg6Base <- Avgoos[which(Avgoos$Store.Type == "BASE STR WAL-MART" & Avgoos$A.B.Region == 6),]
res17 <- mean(rg6Base$FY.15.vs.FY.14.Change)
#Neighborhood Mkt
rg6Neig <- Avgoos[which(Avgoos$Store.Type == "BASE STR NGHBRHD MKT" & Avgoos$A.B.Region == 6),]
res18 <- mean(rg6Neig$FY.15.vs.FY.14.Change)

#Get all subsets for region 7
#Supercenter 
rg7Spr <- Avgoos[which(Avgoos$Store.Type == "BASE STR SUPERCENTER" & Avgoos$A.B.Region == 7),]
res19 <- mean(rg7Spr$FY.15.vs.FY.14.Change)
#Base Walmart 
rg7Base <- Avgoos[which(Avgoos$Store.Type == "BASE STR WAL-MART" & Avgoos$A.B.Region == 7),]
res20 <- mean(rg7Base$FY.15.vs.FY.14.Change)
#Neighborhood Mkt
rg7Neig <- Avgoos[which(Avgoos$Store.Type == "BASE STR NGHBRHD MKT" & Avgoos$A.B.Region == 7),]
res21 <- mean(rg7Neig$FY.15.vs.FY.14.Change)


#Create vector of avg change in percentages for OOS rate
Avg.Change.OOS <- as.vector(c(res1, res2, res3, res4, res5, res6, res7, res8, res9, res10, res11, 
                          res12, res13, res14, res15, res16, res17, res18, res19, res20, res21))
#Create vector of regions 
a <- c("Region 1","Region 2","Region 3", "Region 4", "Region 5", "Region 6", "Region 7")
b <- 3 # Or some other number
a <- sapply(a, function (x) rep(x,b))
Region <- as.vector(a)
#Create vector of store chains
Store.Type <- c("Supercenter", "Walmart", "Neighborhood Market", "Supercenter", "Walmart", "Neighborhood Market",
       "Supercenter", "Walmart", "Neighborhood Market", "Supercenter", "Walmart", "Neighborhood Market",
       "Supercenter", "Walmart", "Neighborhood Market", "Supercenter", "Walmart", "Neighborhood Market",
       "Supercenter", "Walmart", "Neighborhood Market")
#Make ordinal
Store.Type <- factor(Store.Type, levels = c("Supercenter", "Neighborhood Market", "Walmart"))
#Create dataset of the three vectors
avgRes <- data.frame(Avg.Change.OOS, Region, Store.Type)

#Barplot for average change in percentage for OOS rate in FY15 vs FY14 - fix
ch <- ggplot(avgRes, aes(x = Region, y = Avg.Change.OOS))
cha <- ch + geom_bar(stat = "identity") 
chan <- cha + facet_grid(. ~ Store.Type) 
chang <- chan + theme(axis.text.x = element_text(angle = 90, hjust = 1))
chang + ggtitle("Barchart of Avg Change in % for OOS Rate FY15 vs FY14")


#Slide 2
#OOS Rate % cluster 1
os <- ggplot(cl1a, aes(x = FY.15.OOS.Rate))
osr <- os + geom_histogram(aes(fill = ..count..))
osr + ggtitle("Histogram of Cluster 1 OOS Rate %")

#Average cases delivered per week cluster 1 
vo <- ggplot(cl1a, aes(x = Average.Cases.Delivered.Per.Week))
voo <- vo + geom_histogram(aes(fill = ..count..))
voo + ggtitle("Histogram of Cluster 1 Average Cases Delivered Per Week")

#OOS Rate % cluster 2
os <- ggplot(cl2a, aes(x = FY.15.OOS.Rate))
osr <- os + geom_histogram(aes(fill = ..count..))
osr + ggtitle("Histogram of Cluster 2 OOS Rate %")

#Average cases delivered per week cluster 2
vo2 <- ggplot(cl2a, aes(x = Average.Cases.Delivered.Per.Week))
voo2 <- vo2 + geom_histogram(aes(fill = ..count..))
voo2 + ggtitle("Histogram of Cluster 2 Average Cases Delivered Per Week")

#Slide 3 
#Vis9
#Create visualization of segment level 2 
#Get opportunity area through Rate vs Avg Cases Del - done
library(ggplot2)
c <- ggplot(cl1a, aes(x = FY.15.OOS.Rate, y = Average.Cases.Delivered.Per.Week))
ca <- c + geom_point(aes(colour = FY.15.OOS.Rate)) 
car <- ca + geom_point(data = cl1a[c(144, 975),], aes(x = FY.15.OOS.Rate, y = Average.Cases.Delivered.Per.Week), 
                colour = "red", size = 4)
car + ggtitle("OOS Rate % vs Average Cases Delivered Cluster 1")


#Slide 4
#Footage 
#prob1a
ft <- ggplot(prob1a, aes(x = Total.Footage))
fta <- ft + geom_histogram(aes(fill = ..count..))
fta + ggtitle("Histogram of Total Footage in Problematic Stores")
