#Load .csv file
SprCentr <- read.csv("\\\\na1.ofc.loc/dfsusa/HomeDir/Y708514/Home/Data/My Documents/Walmart/Final.csv",
                     na.strings = "", header = TRUE)
#Analyze data
summary(SprCentr)
str(SprCentr)
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
#Take out storenumber column
SprCentr$Store.Number <- NULL
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
#First visualizations
#scatterplot matrix
pairs(~FY.15.OOS.Rate + Total.Footage + Average.Deliveries.Per.Week + 
          Average.Deliveries.Per.Month + Average.Deliveries.Per.Week, data = SprCentr, main = 
          "FY 15 Scatterplot Matrix")
#Plots
#AB Region
plot(SprCentr$A.B.Region, xlab = "AB Regions", ylab = "Frequency", ylim = c(0, 1000))
#Delivery Schedule
par(mar = c(6, 4, 1, 0.001), cex.axis = 0.75)
barplot(table(SprCentr$Delivery.Schedule), xlab = "", ylab = "", 
        las = 2, font.axis = 1.5, ylim = c(0, 1300))
mtext(text = "Delivery Schedule", side = 1, line = 5)
mtext(text = "Frequency", side = 2, line = 3)
#Boxplot
#AB region
plot(SprCentr$A.B.Region, SprCentr$FY.15.OOS.Rate)
#Delivery schedule
plot(SprCentr$Delivery.Schedule, SprCentr$FY.15.OOS.Rate, las = 2,
     ylim = c(0, 0.30))

#Table of categorical variables
#Delivery schedule
table(SprCentr$Delivery.Schedule)
#A.B. region
table(SprCentr$A.B.Region)
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
#Heteroskedastic: NO from res vs fitted
#Outliers: YES from res vs fitted
#Leverage points:


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