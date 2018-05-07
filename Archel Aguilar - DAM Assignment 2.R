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

setwd("C:/Users/arche/Documents/UTS/R-References/R-references-Git/DAM-Assignment-2")
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

#--------------------------------------
#Task 2 - plot line
#--------------------------------------
install.packages("ggplot2")
library(ggplot2)
library(scales)

tnx.i1l1 = subset(tnx.agg, industry=="1" & location=="1")
tnx.i1l1$date = as.Date(tnx.i1l1$date)
str(tnx.i1l1)
boxplot(tnx.i1l1$monthly_amount, data=tnx.i1l1)


ggplot(data = tnx.i1l1, aes(x = date, y = monthly_amount)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y")

#--------------------------------------
#Task 3 - train linear regression on industry 1 and location 1
#--------------------------------------
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


#tnx.i1l1$month = month(tnx.i1l1$date)
#tnx.i1l1$monthseq = as.integer(rownames(tnx.i1l1))
tnx.i1l1$monthseq = getMonthSeq(tnx.i1l1$date)

  
tnx.fit = lm(monthly_amount~monthseq, data=tnx.i1l1)
summary(tnx.fit)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -28924  -7833   1853   7282  18696 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  23355.4    19660.2   1.188    0.241    
# monthseq       801.7      109.5   7.321 3.42e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10180 on 45 degrees of freedom
# Multiple R-squared:  0.5436,	Adjusted R-squared:  0.5334 
# F-statistic: 53.59 on 1 and 45 DF,  p-value: 3.419e-09

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
#?? confidence interval to measure accuracy. 95% confident that monthly_amount will increase between 581 and 1022 per month
#                   2.5 %     97.5 %
#   (Intercept) 141544.2949 153706.414
# monthseq       581.1583   1022.325

#?? MSE - measuring quality of fit. Compare the train MSE with the test MSE using different models
train.mse = mean(tnx.fit$residuals^2)

# or RSE - any prediction will be off by 10180

#?? run a null hypothesis test
# null can be rejected because of low p-value in model (ie. slope / Std Error)

#assessing model accuracy
# -RSE - sqrt(RSS / n-2)
# -R^Squared (IE. 1-(RSS/TSS): RSS is sum of all residuals squared: TSS is sum((monthly_amount - average_monthly_amount)^2)
#  -- rsquared is how close the data is to fit the regression line.

#--------------------------------------
#Task 3c
# - predict amount for December 2016
#--------------------------------------
predict(tnx.fit, data.frame(monthseq=c(getMonthSeq("2016-12-01"))), interval="prediction")
# fit      lwr      upr
# 1 186108.9 164713.5 207504.4

#check prediction
trainpred = as.data.frame(predict(tnx.fit, tnx.i1l1, interval="prediction"))
trainpred = merge(tnx.i1l1, trainpred, by="row.names")
write.csv(trainpred, "TrainingPrediction.csv")

#R squared
cor(trainpred$monthly_amount, trainpred$fit)^2
cor(trainpred$monthly_amount, trainpred$monthseq)^2

#--------------------------------------
#Task 4
# - Apply model to all industries and location
#--------------------------------------

tnx.test = subset(tnx.agg, !(industry=="1" & location=="1"))
tnx.test$date = as.Date(tnx.test$date)
tnx.test$monthseq = getMonthSeq(tnx.test$date)
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
      indloc.fit$lm = lm(monthly_amount~monthseq, data=tempdf)
      
      
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
write.csv(tnx.test, "testresults.csv")


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
      indloc.fit$lm = lm(monthly_amount~monthseq, data=tempdf)
      
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
      
  
      #--------------------------------------
      #Task 5
      # - Predict December 2016
      #--------------------------------------
      
      decPred = as.data.frame(predict(indloc.fit$lm, data.frame(monthseq=c(decSeq)), interval="prediction"))
      indloc$decfit = decPred$fit
      indloc$declwr = decPred$lwr
      indloc$decupr = decPred$upr
    
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
# - Check worse performing industries (ie. industry 2/location 1, industry 1/location 6)
#--------------------------------------

#--------------------------------
#INDUSTRY 2 @ LOCATION 1
#--------------------------------
tnx.i2l1 = subset(tnx.agg, industry=="2" & location=="1")
tnx.i2l1$date = as.Date(tnx.i2l1$date)
tnx.i2l1$monthseq = getMonthSeq(tnx.i2l1$date)
str(tnx.i2l1)
boxplot(tnx.i2l1$monthly_amount, data=tnx.i2l1)

plot(tnx.i2l1$monthseq, tnx.i2l1$monthly_amount)

tnx.i2l1.fit = lm(monthly_amount~monthseq, data=tnx.i2l1)
summary(tnx.i2l1.fit)

abline(tnx.i2l1.fit, lwd=3, col="red")
cor(tnx.i2l1$monthseq, tnx.i2l1$monthly_amount)
#[1] -0.05761972 <-- close to zero

ggplot(data = tnx.i2l1, aes(x = date, y = monthly_amount)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y")
#transaction amount does not increase linearly

#--------------------------------
#INDUSTRY 1 @ LOCATION 6
#--------------------------------
tnx.i1l6 = subset(tnx.agg, industry=="1" & location=="6")
tnx.i1l6$date = as.Date(tnx.i1l6$date)
tnx.i1l6$monthseq = getMonthSeq(tnx.i1l6$date)
str(tnx.i1l6)
boxplot(tnx.i1l6$monthly_amount, data=tnx.i1l6)

plot(tnx.i1l6$monthseq, tnx.i1l6$monthly_amount)

tnx.i1l6.fit = lm(monthly_amount~monthseq, data=tnx.i1l6)
summary(tnx.i1l6.fit)

abline(tnx.i1l6.fit, lwd=3, col="red")
cor(tnx.i1l6$monthseq, tnx.i1l6$monthly_amount)
#[1] 0.03300488 <-- close to zero

ggplot(data = tnx.i1l6, aes(x = date, y = monthly_amount)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y")
#transaction amount does not increase linearly
#could be location was closed for 6 months. Maybe exclude those events.


#------------------------------END--------------


