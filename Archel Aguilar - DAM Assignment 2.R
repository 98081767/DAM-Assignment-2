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
#Task 1 - aggregate data
#--------------------------------------
tnx.agg = aggregate(formula = monthly_amount ~ date:industry:location, data = tnx, FUN = mean)
write.csv(tnx.agg, "monthlyAggregate.csv")
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
#Task 3 - linear regression
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






#--------------------------------------
#Task 3a
#--------------------------------------
plot(tnx.fit)
plot(tnx.i1l1$monthseq, tnx.i1l1$monthly_amount)
abline(tnx.fit, lwd=3, col="red")
cor(tnx.i1l1$monthseq, tnx.i1l1$monthly_amount)

#[1] 0.7372695 <- strong correlation
# R^2 value = 0.5334 <- reasonable fit
# R^2 means - 53% of the variance in monthly amount is explained by the month sequence

#--------------------------------------
#Task 3b
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
# - predict amount for December 2016 (ie. monthseq=48)
#--------------------------------------
predict(tnx.fit, data.frame(monthseq=c(getMonthSeq("2016-12-01"))), interval="prediction")
# fit      lwr      upr
# 1 186108.9 164713.5 207504.4

#check prediction
trainpred = as.data.frame(predict(tnx.fit, tnx.i1l1, interval="prediction"))
trainpred = merge(tnx.i1l1, trainpred, by="row.names")
write.csv(trainpred, "trainingpred.csv")

#R squared
cor(trainpred$monthly_amount, trainpred$fit)^2
cor(trainpred$monthly_amount, trainpred$monthseq)^2

#--------------------------------------
#Task 4
# - 
#--------------------------------------
##?? run predict on test data

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
      
      #summarise accuracy
      indloc$industry = i
      indloc$location = l
      indloc$avg = mean(tempdf$monthly_amount)
      indloc$tss = sum((tempdf$monthly_amount - indloc$avg)^2)
      indloc$rss = sum(tempdf$residual^2)
      indloc$ess = sum((tempdf$fit - indloc$avg)^2)
      indloc$rsq = indloc$ess / indloc$tss
      indloc$cor = cor(tempdf$monthly_amount, tempdf$fit)
      
      #--------------------------------------
      #Task 5
      # - Predict December 2016
      #--------------------------------------
      indloc.fit$industry = i
      indloc.fit$location = l
      indloc.fit$lm = lm(monthly_amount~monthseq, data=tempdf)
      
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

fullsummarydf = as.data.frame(fullsummary)
rownames(fullsummarydf) = NULL

fullsummarydf$industry = unlist(fullsummarydf$industry, use.names=FALSE)
fullsummarydf$location = unlist(fullsummarydf$location, use.names=FALSE)
fullsummarydf$avg =unlist(fullsummarydf$avg, use.names=FALSE)
fullsummarydf$tss= unlist(fullsummarydf$tss, use.names=FALSE)
fullsummarydf$rss=unlist(fullsummarydf$rss, use.names=FALSE)
fullsummarydf$ess=unlist(fullsummarydf$ess, use.names=FALSE)
fullsummarydf$rsq=unlist(fullsummarydf$rsq, use.names=FALSE)
fullsummarydf$cor=unlist(fullsummarydf$cor, use.names=FALSE)
fullsummarydf$decfit=unlist(fullsummarydf$decfit, use.names=FALSE)
fullsummarydf$declwr=unlist(fullsummarydf$declwr, use.names=FALSE)
fullsummarydf$decupr=unlist(fullsummarydf$decupr, use.names=FALSE)

str(fullsummarydf)
write.csv(fullsummarydf, "fullsummary.csv")






#------------------------------END--------------

lm.fit=lm(medv~lstat, data=Boston)
lm.fit


#on the basis of the residual plots there is some evidence for non-linearity.
#leverage statistics can be computed for any number of predictors using hatvalues()
plot(hatvalues(lm.fit))
#tells you which obervation has the largest leverage statistic ie. 375
maxlstat = which.max(hatvalues(lm.fit))
Boston[maxlstat,] #returns row
#you can also tag the extreme observation via identify
myval = identify(Boston$lstat, Boston$medv)


#Findings:
#1. is there a relationship?
# - F-statistic is well over 1
# - p-value is 2e-16 ie. 0.000...2 (16 zeros)
#
#2. how strong is the relationship
# - R2 is 0.5432 (not a strong relationship because R^2 is like a p-value)
# - RSE (average figure for all errors) is 6.216. Means median house value deviates by $6216 ie 6,216/22,532 = 27% 
mean(Boston$medv)
summary(Boston$lstat)
# to obtain confidence interval for the co-efficients you can use the confin() command
# note confidence interval is 2*SE(coeff)
confint(lm.fit)

#predict function can be used to produce confidence intervals and prediction intervals for the prediction of medv for a given value of lstat
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
#fit      lwr      upr
#29.80359 29.00741 30.59978 <- when lstat=5 
#25.05335 24.47413 25.63256 <- when lstat=10
#20.30310 19.73159 20.87461 <- when lstat=15
#
# when lstat = 10 a 95% confidence interval is (24.47, 25.63)

predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")
#fit       lwr      upr
#29.80359 17.565675 42.04151 <- when lstat=5
#25.05335 12.827626 37.27907 <- when lstat=10
#20.30310  8.077742 32.52846 <- when lstat=15
#
# when lstat = 10 a 95% prediction interval is (12.82, 37.27)
 

