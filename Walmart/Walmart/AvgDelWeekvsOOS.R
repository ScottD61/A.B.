#OOS Rate vs Average Deliveries Per Week 
#Create dataset
a <- as.numeric(as.vector(c(0.01, 0.04, 0.06, 0.09, 0.10, 0.25)))
b <- as.numeric(as.vector(c(1, 5, 3, 9, 7, 4)))
c <- data.frame(a, b)
#Create plot
library(ggplot2)
ch <- ggplot(c, aes (x = a, y = b))
cha <- ch + geom_point(aes(colour = a))
cha + ggtitle("A vs B")
