#-----------------------------------------
#Archel Aguilar (980817867)
# DAM - Assignment 2
#
# Data:
#   - Total transaction amounts for your customers each month
#   - Transactions can vary greatly by industry and locations
# Variables
#   - Date - 1/m/yy
#   - Customer ID - identifer
#   - Industry - Code for 10 industries
#   - Location - Code for 10 locations
#   - monthly_amount - numeric
#
# Objective
#   - Predict for monthly_amount next month
#-----------------------------------------


#clear variables
rm(list=ls())

install.packages("ISLR")
library(ISLR)

setwd("C:/Personal/UTS/R-References/R-references-Git/DAM-Assignment-2")
getwd()

tnx = read.csv("transactions.csv")
attach(tnx)

str(tnx)
tnx[1:100,]

#convert industry and location to be factors
tnx$industry = as.factor(tnx$industry)
tnx$location = as.factor(tnx$location)

#convert date to be a date
tnx$date = as.Date(tnx$date, format="%d/%m/%y")

#--------------------------------------
#Task 1 - Aggregate data - with mean on monthly_amount
#--------------------------------------
tnx.agg = aggregate(formula = monthly_amount ~ date:industry:location, data = tnx, FUN = mean)
write.csv(tnx.agg, "MonthlyAggregate.csv")
head(tnx.agg, 20)
str(tnx.agg)

install.packages("lubridate")
library(lubridate)

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}
#elapsed_months(as.Date("2018-05-01"), as.Date("2000-01-01"))
getMonthSeq = function(mydate) {
  elapsed_months(mydate, as.Date("2000-01-01"))
}

getMonthSeq(as.Date("2010-01-01"))


tnx.agg$date = as.Date(tnx.agg$date)
tnx.agg$monthseq = getMonthSeq(tnx.agg$date)


tnx.agg$Season = "Season1"
tnx.agg$Season[which(month(tnx.agg$date)==12 | month(tnx.agg$date)==1 | month(tnx.agg$date)==2)] = "Season1"
tnx.agg$Season[which(month(tnx.agg$date)==3 | month(tnx.agg$date)==4 | month(tnx.agg$date)==5)] = "Season2"
tnx.agg$Season[which(month(tnx.agg$date)==6 | month(tnx.agg$date)==7 | month(tnx.agg$date)==8)] = "Season3"
tnx.agg$Season[which(month(tnx.agg$date)==9 | month(tnx.agg$date)==10 | month(tnx.agg$date)==11)] = "Season4"



#--------------------------------------
#Task 2 - plot line
#--------------------------------------
install.packages("ggplot2")
library(ggplot2)
library(scales)

tnx.i1l1 = subset(tnx.agg, industry=="1" & location=="1")
str(tnx.i1l1)
boxplot(tnx.i1l1$monthly_amount, data=tnx.i1l1)

ggplot(data = tnx.i1l1, aes(x = as.Date(date), y = monthly_amount)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y")

#--------------------------------------
#Task 3 - train linear regression on industry 1 and location 1
#--------------------------------------

tnx.fit = lm(monthly_amount~monthseq:Season, data=tnx.i1l1)
summary(tnx.fit)
# Call:
#   lm(formula = monthly_amount ~ monthseq:Season, data = tnx.i1l1)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -17910  -4602  -1088   5482  17008 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)            32397.44   17600.73   1.841   0.0727 .  
#   monthseq:SeasonSeason1   697.28     100.80   6.917 1.91e-08 ***
#   monthseq:SeasonSeason2   773.62      99.93   7.742 1.29e-09 ***
#   monthseq:SeasonSeason3   758.21      98.28   7.715 1.41e-09 ***
#   monthseq:SeasonSeason4   770.18      96.69   7.966 6.25e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8921 on 42 degrees of freedom
# Multiple R-squared:  0.6732,	Adjusted R-squared:  0.642 
# F-statistic: 21.63 on 4 and 42 DF,  p-value: 9.571e-10

#--------------------------------------
#Task 3a - check how well the model fist the data
#--------------------------------------
install.packages("broom")
library(broom)

tnx.sum = glance(tnx.fit)
tnx.sum$r.squared 
tnx.sum$adj.r.squared
tnx.sum$statistic
tnx.sum$p.value
tnx.sum$sigma #<-- RSE
#summary(tnx.fit)$coefficients[,4]   ##P-values
#summary(tnx.fit)$r.squared          ##R squared values
#summary(tnx.fit)$adj.r.squared        ##R squared values
#summary(tnx.fit)$statistic
#RSE: sqrt(sum(tnx.fit$residuals^2)/45)

plot(tnx.fit)
plot(tnx.i1l1$monthseq, tnx.i1l1$monthly_amount)
abline(tnx.fit, lwd=3, col="red")
cor(tnx.i1l1$monthseq, tnx.i1l1$monthly_amount)

#[1] 0.7372695 <- strong correlation
# R^2 value = 0.5334 <- reasonable fit
# R^2 means - 53% of the variance in monthly amount is explained by the month sequence

#--------------------------------------
#Task 3b - evaluate accuracy 
#--------------------------------------
confint(tnx.fit, level=0.95)
#confidence interval to measure accuracy. 95% confident that monthly_amount will increase between 581 and 1022 per month
#                   2.5 %     97.5 %
#   (Intercept) 141544.2949 153706.414
# monthseq       581.1583   1022.325

#                             2.5 %     97.5 %
#   (Intercept)            -3122.2815 67917.1571
# monthseq:SeasonSeason1   493.8577   900.7099
# monthseq:SeasonSeason2   571.9528   975.2775
# monthseq:SeasonSeason3   559.8755   956.5525
# monthseq:SeasonSeason4   575.0631   965.3068

#MSE - measuring quality of fit. Compare the train MSE with the test MSE using different models
train.mse = mean(tnx.fit$residuals^2)


#RSE - Residual Standard Error is measure of the quality of a linear regression fit
#RSE is the average of residuals  (RSS)
#RSE - sqrt(RSS / n-2)
#Residual standard error: 10180 - any prediction will be off by 10180


#Null hypothesis test
#Null can be rejected because of low p-value in model (ie. slope / Std Error)
#p-value: 3.419e-09 <-- close to 0

#R^Squared - how well the model fits the data
#R^Squared (IE. 1-(RSS/TSS): RSS is sum of all residuals squared: TSS is sum((monthly_amount - average_monthly_amount)^2)
#R^Squared is how close the data is to fit the regression line.
#Adjusted R-squared:  0.5334 


#--------------------------------------
#Task 3c
# - predict amount for December 2016 across different seasons
#--------------------------------------
predict(tnx.fit, data.frame(monthseq=c(getMonthSeq("2016-12-01")), Season=c("Season1", "Season2", "Season3", "Season4")), interval="prediction")
#fit      lwr      upr
#1 173946.1 154164.5 193727.6
#2 189441.3 169838.5 209044.2
#3 186314.9 166900.9 205728.9
#4 188745.0 169493.0 207996.9

#check prediction
trainpred = as.data.frame(predict(tnx.fit, tnx.i1l1, interval="prediction"))
trainpred = merge(tnx.i1l1, trainpred, by="row.names")
write.csv(trainpred, "TrainingPrediction.csv")

#R squared
cor(trainpred$monthly_amount, trainpred$fit)^2
#[1] 0.6731636 <-- this is a better fit than just month seq
cor(trainpred$monthly_amount, trainpred$monthseq)^2
#[1] 0.5435663

#--------------------------------------
#Task 4
# - Apply model to all industries and location
#--------------------------------------

tnx.test = subset(tnx.agg, !(industry=="1" & location=="1"))
tnx.test$date = as.Date(tnx.test$date)
# tnx.test$monthseq = getMonthSeq(tnx.test$date)
tnx.test$rowid = as.integer(rownames(tnx.test))
 
str(tnx.test)
 

tempdf = NULL
testpred = NULL
fullpred = NULL
indloc.fit = NULL

for (i in unique(tnx.test$industry)) {
  print(paste("Industry", i))
  
  for (l in unique(tnx.test$location)) {
    print(paste("--location", l))
    tempdf = NULL
    tempdf = subset(tnx.test, industry==i & location==l)
    if (nrow(tempdf) != 0 ) {
      
      print(nrow(tempdf))
      
      #run model for industry location
      indloc.fit$industry = i
      indloc.fit$location = l
      indloc.fit$lm = lm(monthly_amount~monthseq:Season, data=tempdf)
      
      
      #get predictions per month  
      testpred = predict(indloc.fit$lm, newdata =tempdf, interval="prediction")
      testpred = as.data.frame(testpred)
      testpred$rowid = as.integer(row.names(testpred))
      fullpred = rbind(fullpred, testpred)    
      
    } else {
      print("------no data")
    }
  }
}

str(indloc.fit)

tnx.test = merge(tnx.test, fullpred, by="rowid", all.x = TRUE)
tnx.test$date = as.Date(tnx.test$date)
tnx.test$residual = tnx.test$monthly_amount - tnx.test$fit
write.csv(tnx.test, "TestResults.csv")


tempdf = NULL
indloc = NULL
fullsummary = NULL
indloc.fit = NULL
decSeq = getMonthSeq("2016-12-01")
for (i in unique(tnx.test$industry)) {
  print(paste("Industry", i))
  
  for (l in unique(tnx.test$location)) {
    
    print(paste("--location", l))
    tempdf = NULL
    tempdf = subset(tnx.test, industry==i & location==l)
    if (nrow(tempdf) != 0 ) {
      
      
      indloc.fit$industry = i
      indloc.fit$location = l
      indloc.fit$lm = lm(monthly_amount~monthseq:Season, data=tempdf)
      
      #--------------------------------------
      #Task 4a
      # - Add accurancy measures
      #--------------------------------------
      
      #summarise accuracy
      indloc$industry = i
      indloc$location = l
      
      indloc$avg = mean(tempdf$monthly_amount)
      indloc$c_tss = sum((tempdf$monthly_amount - indloc$avg)^2)
      indloc$c_rss = sum(tempdf$residual^2)
      indloc$c_ess = sum((tempdf$fit - indloc$avg)^2)
      indloc$c_rsq = indloc$c_ess / indloc$c_tss
      indloc$c_cor = cor(tempdf$monthly_amount, tempdf$fit)
      
      indloc.sum = glance(indloc.fit$lm)
      
      indloc$rsq = indloc.sum$r.squared
      indloc$a_rsq = indloc.sum$adj.r.squared
      indloc$fstat = indloc.sum$statistic
      indloc$pval = indloc.sum$p.value
      indloc$rse = indloc.sum$sigma
      
  
      #--------------------------------------
      #Task 5
      # - Predict December 2016
      #--------------------------------------
      
      decPred = as.data.frame(predict(indloc.fit$lm, data.frame(monthseq=c(decSeq), Season=c("Season1")), interval="prediction"))
      indloc$dec_fit = decPred$fit
      indloc$dec_lwr = decPred$lwr
      indloc$dec_upr = decPred$upr
    
      fullsummary = rbind(fullsummary, indloc)    
      
    } else {
      print("------no data")
    }
  }
}


#clean up
fullsummarydf = as.data.frame(fullsummary)
rownames(fullsummarydf) = NULL
for (c in names(fullsummarydf)) {
  fullsummarydf[c] = unlist(fullsummarydf[c], use.names=FALSE)
}

str(fullsummarydf)
write.csv(fullsummarydf, "IndustryLocationSummary.csv")


#--------------------------------------
#Task 4b
# - Check worse performing industries (ie. industry 2/location 1, industry 3/location 1)
#--------------------------------------

#--------------------------------
#INDUSTRY 2 @ LOCATION 1
#--------------------------------
tnx.i2l1 = subset(tnx.agg, industry=="2" & location=="1")
tnx.i2l1$date = as.Date(tnx.i2l1$date)
#tnx.i2l1$monthseq = getMonthSeq(tnx.i2l1$date)
str(tnx.i2l1)
boxplot(tnx.i2l1$monthly_amount/1000, data=tnx.i2l1)
hist(tnx.i2l1$monthly_amount/1000)

tnx.i2l1.fit = lm(monthly_amount~monthseq:Season, data=tnx.i2l1)
summary(tnx.i2l1.fit)
# Call:
#   lm(formula = monthly_amount ~ monthseq:Season, data = tnx.i2l1)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -47767 -17935  -3528  14556  65600 
# 
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            463499.0    56410.6   8.217  2.8e-10 ***
# monthseq:SeasonSeason1   -186.0      323.1  -0.576    0.568    
# monthseq:SeasonSeason2   -163.0      320.3  -0.509    0.613    
# monthseq:SeasonSeason3   -188.5      315.0  -0.599    0.553    
# monthseq:SeasonSeason4   -125.8      309.9  -0.406    0.687    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 28590 on 42 degrees of freedom
# Multiple R-squared:  0.03065,	Adjusted R-squared:  -0.06166 
# F-statistic: 0.332 on 4 and 42 DF,  p-value: 0.8548



plot(tnx.i2l1$monthseq, tnx.i2l1$monthly_amount)
abline(tnx.i2l1.fit, lwd=3, col="red")
cor(tnx.i2l1$monthseq, tnx.i2l1$monthly_amount)
#[1] -0.05761972 <-- close to zero

ggplot(data = tnx.i2l1, aes(x = date, y = monthly_amount)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y")
#transaction amount does not increase linearly - could be quadratic

#--------------------------------
#INDUSTRY 3 @ LOCATION 1
#--------------------------------
tnx.i3l1 = subset(tnx.agg, industry=="3" & location=="1")
tnx.i3l1$date = as.Date(tnx.i3l1$date)
#tnx.i3l1$monthseq = getMonthSeq(tnx.i3l1$date)
str(tnx.i3l1)
boxplot(tnx.i3l1$monthly_amount/1000, data=tnx.i3l1)
hist(tnx.i3l1$monthly_amount/1000)

tnx.i3l1.fit = lm(monthly_amount~monthseq:Season, data=tnx.i3l1)
summary(tnx.i3l1.fit)
# Call:
#   lm(formula = monthly_amount ~ monthseq:Season, data = tnx.i3l1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -184923  -84486    1588   62693  197818 
# 
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)  
# (Intercept)              474172     201599   2.352   0.0234 *
# monthseq:SeasonSeason1     1101       1155   0.954   0.3457  
# monthseq:SeasonSeason2     1093       1145   0.955   0.3449  
# monthseq:SeasonSeason3     1016       1126   0.903   0.3719  
# monthseq:SeasonSeason4     1140       1108   1.029   0.3093  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 102200 on 42 degrees of freedom
# Multiple R-squared:  0.03054,	Adjusted R-squared:  -0.06179 
# F-statistic: 0.3308 on 4 and 42 DF,  p-value: 0.8557

plot(tnx.i3l1$monthseq, tnx.i3l1$monthly_amount)
abline(tnx.i3l1.fit, lwd=3, col="red")
cor(tnx.i3l1$monthseq, tnx.i3l1$monthly_amount)
#[1] 0.153416

ggplot(data = tnx.i3l1, aes(x = date, y = monthly_amount)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y")
#transaction amount does not increase linearly - quadratic
#could be location was closed for 6 months. Maybe exclude those events.


#------------------------------END--------------


