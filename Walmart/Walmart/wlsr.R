#Get wholesalers corresponding to each stores in problematic areas in cluster 1
#Load whlsr csv file with names from WalmartOOS.xlsx file
Whslr <- read.csv("/Users/scdavis6/A.B./Walmart/Walmart/whlsr.csv", na.strings = "", header = TRUE)
#Feature engineering
#Convert store number to factor
Whslr$Store.Number <- as.numeric(Whslr$Store.Number)

#Merge wholesalers to entire dataset 
AllWhslrdata <- merge(Whslr, SprCentr, by = "Store.Number")
#Export to csv
write.csv(AllWhslrdata, file = "/Users/scdavis6/A.B./Walmart/Walmart/AllWhslrData.csv")

#Merge wholesalers to segmentation level 1
Level1 <- merge(Whslr, cl1a, by = "Store.Number")
#Export to csv
write.csv(Level1, file = "/Users/scdavis6/A.B./Walmart/Walmart/Level1.csv")

#Merge wholesalers to segmentation level 2 no prob
probWhslrdata <- merge(Whslr, prob1ab, by = "Store.Number")
#Export as csv file
write.csv(probWhslrdata, file = "/Users/scdavis6/A.B./Walmart/Walmart/probWhslrRes.csv")

#Merge wholesalers to segmentation level 2 prob
Whslrdata <- merge(Whslr, prob1a, by = "Store.Number")
#Export as csv file 
write.csv(Whslrdata, file = "/Users/scdavis6/A.B./Walmart/Walmart/WhslrRes.csv")

#Merge wholesalers to segmentation level 3
level3 <- merge(Whslr, prob1aft, by = "Store.Number")
#Export as csv file
write.csv(level3, file = "/Users/scdavis6/A.B./Walmart/Walmart/Level3.csv")



#Exported to Randy 
#Combine wholesaler data with problematic cluster 1 data, segment level 2
finalseg2 <- merge(Whslrdata[, 1:3], prob1a, by = "Store.Number")
#Problem because there were three stores in Walmart OOS w/o a wholesaler name BUT had an OOS Rate 
#I only wanted stores w/ an OOS rate, NOT with OOS rate AND wholesaler name 
#OR same wholesaler to multiple stores
#Wholesalers pushing out top five highest volume segment level 2
Whslr5seg2 <- finalseg2[order(finalseg2$Average.Cases.Delivered.Per.Week, decreasing = T)[1:5],]
#Save as .csv - not essential 
write.csv(Whslr5seg2, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/seg2whslr.csv")

#Get wholesaler data for segment level 3
#Problematic in cluster 1 w/ feet constraint 
finalseg4 <- merge(Whslrdata[, 1:3], prob1aft, by = "Store.Number")
#Wholesalers serving non-prob in cluster 1 w/ feet constraint - don't need 
whslrseg4 <- merge(probWhslrdata[, 1:3], prob1abft, by = "Store.Number")

#Top five wholesalers by volume prob feet constraint - lvl three 
Whslrproseg3 <- finalseg4[order(finalseg4$Average.Cases.Delivered.Per.Week, decreasing = T)[1:5],]
#Export to .csv
write.csv(Whslrproseg3, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/probfeet5.csv")
#Top five wholesalers by volume non-prob without feet constraint 
Whslrnonseg3 <- probWhslrdata[order(probWhslrdata$Average.Cases.Delivered.Per.Week, decreasing = T)[1:5],]
#Export to .csv
write.csv(Whslrnonseg3, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/nonprobfeet5.csv")


#Top 125 wholesalers by volume segment level three 
Whslrproseg3.5 <- finalseg4[order(finalseg4$Average.Cases.Delivered.Per.Week, decreasing = T)[1:125],]
#Export problem to .csv file
write.csv(Whslrproseg3.5, file = "/Users/scdavis6/A.B./Walmart/Walmart/probfeet.csv")
#Top 150 wholesalers by volume non-prob w/o feet constraint - segment level 3 
Whslrnonseg3.5 <- probWhslrdata[order(probWhslrdata$Average.Cases.Delivered.Per.Week, decreasing = T)[1:125],]
#Export non-problem to .csv file
write.csv(Whslrnonseg3.5, file = "/Users/scdavis6/A.B./Walmart/Walmart/nonprobfeet.csv")


#Combine wholesaler data with problematic cluster 1 data. segment level 4
finalseg3 <- merge(Whslrdata[, 1:3], prob5, by = "Store.Number")
#Wholesalers in segment level 4 pushing out top five highest volume 
Whslr5seg3 <- finalseg3[order(finalseg3$Average.Cases.Delivered.Per.Week, decreasing = T)[1:5],]
#Save to .csv
write.csv(Whslr5seg3, file = "//na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/seg3whslr.csv")


#Get wholesalers for cluster 2, second level of segmentation 
Whslrdata <- merge(Whslr, prob1a, by = "Store.Number")

