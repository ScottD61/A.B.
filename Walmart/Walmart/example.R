#Create dataset
#avg value
Avg.Value <- as.vector(as.numeric(c(1:11,15,14,13,12,11,10,9,8,7,6)))
#Regions
ab <- c("Region 1","Region 2","Region 3", "Region 4", "Region 5", "Region 6", "Region 7")
b <- 3 # Or some other number
ab <- sapply(ab, function (x) rep(x,b))
Region <- as.vector(ab)
#Chains
Store.Type <- c("Supercenter", "Mart", "Market", "Supercenter", "Mart", "Market", 
                "Supercenter", "Mart", "Market", "Supercenter", "Mart", "Market",
                "Supercenter", "Mart", "Market", "Supercenter", "Mart", "Market",
                "Supercenter", "Mart", "Market")
#Make into ordinal
Store.Type <- factor(Store.Type, levels = c("Supercenter", "Mart", "Market"))
#Combine dataset
avgRes <- data.frame(Avg.Value, Region, Store.Type)
#Graph - colors fine for now
library(ggplot2)
ch <- ggplot(avgRes, aes(x = Region, y = Avg.Value, colour = Store.Type))
cha <- ch + geom_bar(stat = "identity") 
chan <- cha + facet_grid(. ~ Store.Type) 
chang <- chan + theme(axis.text.x = element_text(angle = 90, hjust = 1))
chang + ggtitle("Chart")



#Solution posted online
avgRes$ordvar <- paste(avgRes$Store.Type, avgRes$Region, sep = "_")

#Graph
library(ggplot2)
ch <- ggplot(avgRes, aes(x = reorder(ordvar,Avg.Value), y = Avg.Value)) # reorder ordvar by value
cha <- ch + geom_bar(stat = "identity") 
chan <- cha + facet_grid(. ~ Store.Type, scale="free_x") #added "free_x", or you will have a lot of blank values in your plot
chang <- chan + theme(axis.text.x = element_text(angle = 90, hjust = 1))
chang + ggtitle("Chart")




#Problem 1 down - I got ordering by facet_grid by converting to ordinal variables
#Problem 2 down 
