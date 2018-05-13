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

boxplot(monthly_amount/1000~industry:location, data=tnx.agg, las=2, horizontal=TRUE, ylab="Industry Locations", xlab="Monthly Amount ($,000s)")


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
tnx.agg$year = year(tnx.agg$date)

tnx.agg$Season = "Season1"
tnx.agg$Season[which(month(tnx.agg$date)==12 | month(tnx.agg$date)==1 | month(tnx.agg$date)==2)] = "Season1"
tnx.agg$Season[which(month(tnx.agg$date)==3 | month(tnx.agg$date)==4 | month(tnx.agg$date)==5)] = "Season2"
tnx.agg$Season[which(month(tnx.agg$date)==6 | month(tnx.agg$date)==7 | month(tnx.agg$date)==8)] = "Season3"
tnx.agg$Season[which(month(tnx.agg$date)==9 | month(tnx.agg$date)==10 | month(tnx.agg$date)==11)] = "Season4"


str(tnx.agg)

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

#model 1
#xmodel = "monthly_amount~monthseq"
#MSE = 99,314,297

# Residuals:
#   Min     1Q Median     3Q    Max 
# -28924  -7833   1853   7282  18696 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  23355.4    19660.2   1.188    0.241    
# monthseq       801.7      109.5   7.321 3.42e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10180 on 45 degrees of freedom
# Multiple R-squared:  0.5436,	Adjusted R-squared:  0.5334 
# F-statistic: 53.59 on 1 and 45 DF,  p-value: 3.419e-09

#model 2 
#xmodel = "monthly_amount~monthseq + Season"
#MSE = 71,714,910

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -18906.5  -4405.1   -822.6   5646.5  15955.5 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   22554.89   17511.74   1.288 0.204802    
# monthseq        753.73      98.46   7.655 1.71e-09 ***
#   SeasonSeason2 13434.72    3741.53   3.591 0.000857 ***
#   SeasonSeason3 10654.42    3763.02   2.831 0.007086 ** 
#   SeasonSeason4 12702.95    3807.38   3.336 0.001784 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8958 on 42 degrees of freedom
# Multiple R-squared:  0.6704,	Adjusted R-squared:  0.639 
# F-statistic: 21.36 on 4 and 42 DF,  p-value: 1.137e-09

#model 3
xmodel = "monthly_amount~monthseq:Season"
#MSE = 71,115,531

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


#model 4
#xmodel = "monthly_amount~monthseq:Season + year:Season"
#MSE = 69,466,494
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -17080.6  -6066.1    568.1   6946.3  18024.1 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)
# (Intercept)            -2549498.7 13793802.7  -0.185    0.854
# monthseq:SeasonSeason1      511.0      583.7   0.875    0.387
# monthseq:SeasonSeason2      594.0      606.8   0.979    0.334
# monthseq:SeasonSeason3      645.4      606.8   1.064    0.294
# monthseq:SeasonSeason4      816.2      606.9   1.345    0.187
# SeasonSeason1:year         1298.0     6894.9   0.188    0.852
# SeasonSeason2:year         1297.5     6897.6   0.188    0.852
# SeasonSeason3:year         1291.7     6898.5   0.187    0.852
# SeasonSeason4:year         1277.4     6899.3   0.185    0.854
# 
# Residual standard error: 9269 on 38 degrees of freedom
# Multiple R-squared:  0.6807,	Adjusted R-squared:  0.6135 
# F-statistic: 10.13 on 8 and 38 DF,  p-value: 1.977e-07



#tnx.i1l1$month = month(tnx.i1l1$date)
#xmodel = "monthly_amount~month:Season"

tnx.fit = lm(xmodel, data=tnx.i1l1)
summary(tnx.fit)

#MSE - measuring quality of fit. Compare the train MSE with the test MSE using different models
mean(tnx.fit$residuals^2)


#install.packages("jtools")
#library(jtools)
#interact_plot(tnx.fit, pred = "monthseq", modx="monthseq")
#cat_plot(tnx.fit, pred="Season", modx = "date")

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
#cannot compare MSE with other locations due to transaction amount variance between industries and locations.


#RSE - Residual Standard Error is measure of the quality of a linear regression fit
#RSE is the average of residuals  (RSS)
#RSE - sqrt(RSS / n-2)
#Residual standard error: 8921 - any prediction will be off by 8921


#Null hypothesis test
#Null can be rejected because of low p-value in model (ie. slope / Std Error)
#p-value: 3.419e-09 <-- close to 0

#R^Squared - how well the model fits the data (goodness of fit)
#R^Squared (IE. 1-(RSS/TSS): RSS is sum of all residuals squared: TSS is sum((monthly_amount - average_monthly_amount)^2)
#R^Squared is how close the data is to fit the regression line.
#Adjusted R-squared:  0.5334 


#--------------------------------------
#Task 3c
# - predict amount for December 2016 across different seasons
#--------------------------------------
predict(tnx.fit, data.frame(monthseq=c(getMonthSeq("2016-12-01")), Season=c("Season1")), interval="prediction")
#fit      lwr      upr
#1 173946.1 154164.5 193727.6


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
      indloc.fit$lm = lm(xmodel, data=tempdf)
      
      
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
      indloc.fit$lm = lm(xmodel, data=tempdf)
      
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
      #indloc$c_mse = mean(tempdf$residual^2)
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
tnx.i2l1$monthly_amount_t = tnx.i2l1$monthly_amount/1000

str(tnx.i2l1)
boxplot(tnx.i2l1$monthly_amount_t, data=tnx.i2l1)
hist(tnx.i2l1$monthly_amount_t)

tnx.i2l1.fit = lm(xmodel, data=tnx.i2l1)
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

ggplot(data = tnx.i2l1, aes(x = date, y = monthly_amount_t)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y") + labs(y="Monthly Amount ($000s)")
#transaction amount does not increase linearly - could be quadratic

#--------------------------------
#INDUSTRY 3 @ LOCATION 1
#--------------------------------
tnx.i3l1 = subset(tnx.agg, industry=="3" & location=="1")
tnx.i3l1$date = as.Date(tnx.i3l1$date)
#tnx.i3l1$monthseq = getMonthSeq(tnx.i3l1$date)
tnx.i3l1$monthly_amount_t = tnx.i3l1$monthly_amount/1000
str(tnx.i3l1)
boxplot(tnx.i3l1$monthly_amount/1000, data=tnx.i3l1)
hist(tnx.i3l1$monthly_amount/1000)

tnx.i3l1.fit = lm(xmodel, data=tnx.i3l1)
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

ggplot(data = tnx.i3l1, aes(x = date, y = monthly_amount_t)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y") + labs(y="Monthly Amount ($000s)")
#transaction amount does not increase linearly - quadratic
#could be location was closed for 6 months. Maybe exclude those events.


#--------------------------------
#INDUSTRY 5 @ LOCATION 5 - OUTLIER EXAMPLE
#--------------------------------
tnx.i5l5 = subset(tnx.agg, industry=="5" & location=="5")
tnx.i5l5$date = as.Date(tnx.i5l5$date)
tnx.i5l5$year = year(tnx.i5l5$date)
tnx.i5l5$month = month(tnx.i5l5$date)
tnx.i5l5$monthly_amount_t = tnx.i5l5$monthly_amount/1000
#year(tnx.i5l5$date)
#tnx.i5l5$monthseq = getMonthSeq(tnx.i5l5$date)
str(tnx.i5l5)
boxplot(tnx.i5l5$monthly_amount_t, data=tnx.i5l5)
hist(tnx.i5l5$monthly_amount_t)

xmodel
tnx.i5l5.fit = lm(xmodel, data=tnx.i5l5)
summary(tnx.i5l5.fit)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -107124  -23385   -7830   10887  446808 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)  
#   (Intercept)            -125625.9   157392.5  -0.798   0.4294  
#   monthseq:SeasonSeason1    1839.1      892.6   2.060   0.0457 *
#   monthseq:SeasonSeason2    1893.3      893.1   2.120   0.0401 *
#   monthseq:SeasonSeason3    1933.1      878.3   2.201   0.0334 *
#   monthseq:SeasonSeason4    2097.2      864.1   2.427   0.0197 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 77460 on 41 degrees of freedom
# Multiple R-squared:  0.1737,	Adjusted R-squared:  0.09313 
# F-statistic: 2.155 on 4 and 41 DF,  p-value: 0.09121

plot(tnx.i5l5$monthseq, tnx.i5l5$monthly_amount)
abline(tnx.i5l5.fit, lwd=3, col="red")
cor(tnx.i5l5$monthseq, tnx.i5l5$monthly_amount)
#0.3580094


ggplot(data = tnx.i5l5, aes(x = date, y = monthly_amount_t)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y") + labs(y="Monthly Amount ($000s)")
#data has one major outlier
boxplot(tnx.i5l5$monthly_amount)

#--------------------------------
#INDUSTRY 10 @ LOCATION 3
#--------------------------------
tnx.i10l3 = subset(tnx.agg, industry=="10" & location=="3")
tnx.i10l3$date = as.Date(tnx.i10l3$date)
tnx.i10l3$year = year(tnx.i10l3$date)
tnx.i10l3$month = month(tnx.i10l3$date)
tnx.i10l3$monthly_amount_t = tnx.i10l3$monthly_amount/1000
#year(tnx.i10l3$date)
#tnx.i10l3$monthseq = getMonthSeq(tnx.i10l3$date)
str(tnx.i10l3)
boxplot(tnx.i10l3$monthly_amount_t, data=tnx.i10l3)
hist(tnx.i10l3$monthly_amount_t)

xmodel
tnx.i10l3.fit = lm(xmodel, data=tnx.i10l3)
summary(tnx.i10l3.fit)
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3824  -2100      3   1213   4852 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            460676.1    60596.7   7.602 0.000126 ***
#   monthseq:SeasonSeason1  -2350.3      378.6  -6.209 0.000442 ***
#   monthseq:SeasonSeason2  -2332.0      381.3  -6.116 0.000483 ***
#   monthseq:SeasonSeason3  -2326.5      374.2  -6.217 0.000438 ***
#   monthseq:SeasonSeason4  -2306.6      367.4  -6.277 0.000413 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3383 on 7 degrees of freedom
# Multiple R-squared:  0.8801,	Adjusted R-squared:  0.8116 
# F-statistic: 12.85 on 4 and 7 DF,  p-value: 0.002434

plot(tnx.i10l3$monthseq, tnx.i10l3$monthly_amount)
abline(tnx.i10l3.fit, lwd=3, col="red")
cor(tnx.i10l3$monthseq, tnx.i10l3$monthly_amount)
#0.3580094


ggplot(data = tnx.i10l3, aes(x = date, y = monthly_amount_t)) + geom_line(colour="skyblue", size=1.5) + scale_x_date(date_breaks="1 year", date_labels = "%b %y") + labs(y="Monthly Amount ($000s)")
#data has one major outlier
boxplot(tnx.i10l3$monthly_amount)


#------------------------------END--------------

