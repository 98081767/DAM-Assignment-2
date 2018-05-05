#ISLR Lab - 3.6 Linear Regression

library(MASS)
library(ISLR)
attach(Boston)

#create function
LoadLibraries=function() {
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries()


#show boston data - medv (median house value) for 506 neighbourhoods around Boston
#we seek to predict medv using 13 predictors such as:
# - rm (average number of rooms per house)
# - age (average age of houses)
# - lstat (percent of households with low socioeconomic status)
# - nox - nitrogen oxides (pollution)
fix(Boston)

names(Boston)
?Boston

#start with fitting a simple linear regression model with medv as the response and lstat as the predictor
#lm(y~x, data) y = response, x= predictor

lm.fit=lm(medv~lstat, data=Boston)
lm.fit


#interesting - this plots the linear model
plot(lm.fit)
plot(Boston$lstat, Boston$medv)
abline(lm.fit)
#other options for line and plot
abline(lm.fit, lwd=3, col="red")
plot(Boston$lstat, Boston$medv, col="red") #dot colour
plot(Boston$lstat, Boston$medv, pch=20) #dot size
plot(Boston$lstat, Boston$medv, pch="+") #dot shape
plot(1:20,1:20,pch=1:20) #dot shapes
#par allows you to view multiple plots at the same time
par(mfrow=c(2,2))
plot(lm.fit)
#to view residuals
plot(predict(lm.fit), residuals(lm.fit))
#to view studentised residuals
plot(predict(lm.fit), rstudent(lm.fit))

#on the basis of the residual plots there is some evidence for non-linearity.
#leverage statistics can be computed for any number of predictors using hatvalues()
plot(hatvalues(lm.fit))
#tells you which obervation has the largest leverage statistic ie. 375
maxlstat = which.max(hatvalues(lm.fit))
Boston[maxlstat,] #returns row
#you can also tag the extreme observation via identify
myval = identify(Boston$lstat, Boston$medv)




summary(lm.fit)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-15.168  -3.990  -1.318   2.034  24.500 
#
#Coefficients:
#             Estimate      Std. Error  t value   Pr(>|t|)    
#(Intercept)  34.55384(b)   0.56263     61.41     <2e-16 ***
#  lstat      -0.95005(m)   0.03873     -24.53    <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 6.216 on 504 degrees of freedom
#Multiple R-squared:  0.5441,	Adjusted R-squared:  0.5432 
#F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

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

#-------------------------------------
#MULTIPLE LINEAR REGRESSION
#-------------------------------------
#use lm() function - lm(y~x1+x2+x3) with 3 predictors
mlr = lm(medv~lstat+age, data=Boston)
summary(mlr)
#Call:
#  lm(formula = medv ~ lstat + age, data = Boston)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-15.981  -3.978  -1.283   1.968  23.158 
#
#Coefficients:
#               Estimate  Std. Error  t value   Pr(>|t|)    
#(Intercept)    33.22276    0.73085    45.458   < 2e-16 ***
#  lstat        -1.03207    0.04819   -21.416   < 2e-16 ***
#  age          0.03454     0.01223     2.826   0.00491 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 6.173 on 503 degrees of freedom
#Multiple R-squared:  0.5513,	Adjusted R-squared:  0.5495 
#F-statistic:   309 on 2 and 503 DF,  p-value: < 2.2e-16

#is there a relationship between the response and predictors?
# - The Fstatistic is greater than 1 and the associated p-value is less than 1%
# - This means you can reject the null hypothesis because at least one of the predictors is related to medv

#Assess the predictors. ie. are predictors affecting each other
mdf = data.frame(medv=Boston$medv, lstat=Boston$lstat, age=Boston$age)
fix(mdf)
cor(mdf)
#         medv        lstat       age
#medv     1.0000000   -0.7376627  -0.3769546
#lstat    -0.7376627  1.0000000   0.6023385
#age      -0.3769546  0.6023385   1.0000000

#corelation of 0.602 shows there is a relationship between lstat and age. 
plot(mdf$age, mdf$lstat)

mdf.fit = lm(lstat~age, data=mdf)
#abline(lm(lstat~age, data=mdf))
abline(mdf.fit)
cor(mdf$lstat, mdf$age)
summary(mdf.fit)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-15.2600  -3.3552  -0.1492   2.6266  25.8781 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.17435    0.66856   3.252  0.00122 ** 
#  age          0.15281    0.00902  16.940  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 5.706 on 504 degrees of freedom
#Multiple R-squared:  0.3628,	Adjusted R-squared:  0.3615 
#F-statistic:   287 on 1 and 504 DF,  p-value: < 2.2e-16

#the relationship between is as average house age increases so do lstat (lower socio ecomonic area)

#short cut to do multiple linear regression will all variables
mdf2.fit = lm(medv~., data=Boston)
summary(mdf2.fit)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-15.595  -2.730  -0.518   1.777  26.199 

#Coefficients:
#               Estimate   Std. Error t value Pr(>|t|)    
#(Intercept)    3.646e+01  5.103e+00   7.144  3.28e-12 ***
#  crim        -1.080e-01  3.286e-02  -3.287  0.001087 ** 
#  zn           4.642e-02  1.373e-02   3.382  0.000778 ***
#  indus        2.056e-02  6.150e-02   0.334  0.738288    
#  chas         2.687e+00  8.616e-01   3.118  0.001925 ** 
#  nox         -1.777e+01  3.820e+00  -4.651  4.25e-06 ***     <- strong affect on price because estimate is high
#  rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***      <- strong affect on price because estimate is high
#  age          6.922e-04  1.321e-02   0.052  0.958229    
#  dis         -1.476e+00  1.995e-01  -7.398  6.01e-13 ***     <- strong affect on price because estimate is high
#  rad          3.060e-01  6.635e-02   4.613  5.07e-06 ***
#  tax         -1.233e-02  3.760e-03  -3.280  0.001112 ** 
#  ptratio     -9.527e-01  1.308e-01  -7.283  1.31e-12 ***
#  black        9.312e-03  2.686e-03   3.467  0.000573 ***
#  lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 4.745 on 492 degrees of freedom
#Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
#F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16
summary(mdf2.fit)$r.sq #gives r squared
summary(mdf2.fit)$sigma #gives RSE

#if want to run a linear regression on all variables except 1
mdf2.fit = lm(medv~.-age-indus, data=Boston)
summary(mdf2.fit)

pairs(Boston[,])

confint(mdf2.fit)


#preduct medv with multiple predictor model
#predict function can be used to produce confidence intervals and prediction intervals for the prediction of medv for a given value of lstat
predict(mdf2.fit, data.frame(crim=c(5,10,15),age=c(5,10,15),zn=0,chas=0,nox=0,rm=0,dis=0,rad=0,tax=0,ptratio=0,black=0,lstat=0,indus=0), interval="confidence")
predict(mdf2.fit, data.frame(crim=c(5,10,15),age=0,zn=0,chas=0,nox=0,rm=0,dis=0,rad=0,tax=0,ptratio=0,black=0,lstat=0,indus=0), interval="prediction")

summary(Boston$medv)

attach(Boston)
#Results based on nox, rm and dis, the strong predictors. These 3 account for more than half (0.5448) of the relationship 
#compared with all the variables.

mdf8.fit = lm(medv~nox+rm+dis, data=Boston)
summary(mdf8.fit)
plot(mdf8.fit)

#Call:
#  lm(formula = medv ~ nox + rm + dis, data = Boston)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-17.462  -3.070  -0.624   2.597  38.643 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -10.1181     4.1557  -2.435  0.01525 *  
#  nox         -28.3433     3.8441  -7.373 6.92e-13 ***
#  rm            8.0972     0.4139  19.563  < 2e-16 ***
#  dis          -0.6627     0.2060  -3.217  0.00138 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 6.224 on 502 degrees of freedom
#Multiple R-squared:  0.5448,	Adjusted R-squared:  0.5421 
#F-statistic: 200.3 on 3 and 502 DF,  p-value: < 2.2e-16


summary(Boston)
#medv - q1=17, mean=22.53, q3=25
#nox - q3=0.62, mean = 0.55, q3=0.44 <-switched q3 and q1 because the more nox the worse the price
#rm - q1=5.8, mean = 6, q4=6.62
#dis - q3=5.18, mean = 3.79, q1=2.1 <-switched q3 and q1 because the further away from cbd the worse the price

predict(mdf8.fit, data.frame(nox=c(0.62,0.55,0.44),rm=c(5.8,6,6.62),dis=c(5.18,3.79,2.1)), interval="confidence")
#   fit      lwr      upr
#1 15.83983 14.69174 16.98793
#2 20.36448 19.76929 20.95967 <- if you are predicting the average for nox,rm and dis it appears you get a lower medv than the current mean of 22.53
#3 29.62250 28.08420 31.16080 <- assuming best case the price greatly increases mean of medv from 22.53 to 29.62



#-------------------------------------
# INTERACTION TERMS
# used when combining predictors
#-------------------------------------
summary(lm(medv~lstat*age,data=Boston))
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-15.806  -4.045  -1.333   2.085  27.552 

#Coefficients:
#               Estimate    Std. Error t value  Pr(>|t|)    
#(Intercept)    36.0885359  1.4698355  24.553   < 2e-16 ***
#   lstat       -1.3921168  0.1674555  -8.313   8.78e-16 ***
#   age         -0.0007209  0.0198792  -0.036   0.9711    
#   lstat:age    0.0041560  0.0018518   2.244   0.0252 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 6.149 on 502 degrees of freedom
#Multiple R-squared:  0.5557,	Adjusted R-squared:  0.5531 
#F-statistic: 209.3 on 3 and 502 DF,  p-value: < 2.2e-16


#-------------------------------------
# NON-LINEAR TRANSFORMATION OF THE PREDICTORS
# eg. squaring a predictor
# the function I() is needed since ^ has special meaning
#-------------------------------------
mdf3.fit = lm(medv~lstat+I(lstat^2), data=Boston)
summary(mdf3.fit)
# The near zero p-value associated with quadratic term (lstat^2) suggests it's a better model
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-15.2834  -3.8313  -0.5295   2.3095  25.4148 

#Coefficients:
#               Estimate    Std. Error t value Pr(>|t|)    
#(Intercept)    42.862007   0.872084   49.15   <2e-16 ***
#  lstat        -2.332821   0.123803  -18.84   <2e-16 ***
#  I(lstat^2)   0.043547    0.003745   11.63   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 5.524 on 503 degrees of freedom
#Multiple R-squared:  0.6407,	Adjusted R-squared:  0.6393 
#F-statistic: 448.5 on 2 and 503 DF,  p-value: < 2.2e-16


par(mfrow=c(2,2))
plot(mdf3.fit) #compare plots
plot(lm.fit) #compare plots
plot(Boston$lstat+I(Boston$lstat^2), Boston$medv)
cor(Boston$lstat+I(Boston$lstat^2), Boston$medv)
#compare with
plot(Boston$lstat, Boston$medv)
cor(Boston$lstat, Boston$medv)


summary(lm.fit)
#Coefficients:
#             Estimate      Std. Error  t value   Pr(>|t|)    
#(Intercept)  34.55384(b)   0.56263     61.41     <2e-16 ***
#  lstat      -0.95005(m)   0.03873     -24.53    <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 6.216 on 504 degrees of freedom
#Multiple R-squared:  0.5441,	Adjusted R-squared:  0.5432 
#F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

#--------------------------
# The anova() fuction can be used to compare which model is better. The quadratic or linear
#--------------------------
# Model 2 is better because:
#   - F-statistic is 135 
#   - p-value is close to 0, 
#   - RSS (sum of all differences to the least squares line) is lower.
#   - RSE (average of all the errors) is lower
# this is not surprising given than there is some evidence for non-linearity in the relationship between mdev and lstat.
anova(lm.fit,mdf3.fit)
#Analysis of Variance Table

#Model 1: medv ~ lstat
#Model 2: medv ~ lstat + I(lstat^2)
#   Res.Df   RSS  Df  Sum of Sq     F    Pr(>F)    
#1  504     19472                                 
#2  503     15347  1  4125.1      135.2   < 2.2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#--------------------------
# CUBIC FIT or polynomials
# Rather than use I(X^3) for a cubic fit you can use poly instead
#--------------------------
mdf4.fit = lm(medv~poly(lstat,5), data=Boston)
summary(mdf4.fit)
plot(mdf4.fit)
#Call:
#  lm(formula = Boston$medv ~ poly(Boston$lstat, 5))

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-13.5433  -3.1039  -0.7052   2.0844  27.1153 

#Coefficients:
#                         Estimate    Std. Error t value  Pr(>|t|)    
#(Intercept)                22.5328     0.2318    97.197  < 2e-16 ***
#  poly(Boston$lstat, 5)1 -152.4595     5.2148   -29.236  < 2e-16 ***
#  poly(Boston$lstat, 5)2   64.2272     5.2148    12.316  < 2e-16 ***
#  poly(Boston$lstat, 5)3  -27.0511     5.2148    -5.187  3.10e-07 ***
#  poly(Boston$lstat, 5)4   25.4517     5.2148     4.881  1.42e-06 ***
#  poly(Boston$lstat, 5)5  -19.2524     5.2148    -3.692  0.000247 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 5.215 on 500 degrees of freedom
#Multiple R-squared:  0.6817,	Adjusted R-squared:  0.6785 
#F-statistic: 214.2 on 5 and 500 DF,  p-value: < 2.2e-16

anova(lm.fit, mdf3.fit, mdf4.fit)
#Analysis of Variance Table
# model 3 is better because the RSS is smaller

#Model 1: medv ~ lstat
#Model 2: medv ~ lstat + I(lstat^2)
#Model 3: medv ~ poly(lstat, 5)
#   Res.Df   RSS  Df    Sum of Sq       F     Pr(>F)    
#1    504   19472                                   
#2    503   15347  1    4125.1      151.693   < 2.2e-16 ***
#3    500   13597  3    1750.2       21.453   4.372e-13 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# if you go to the 6th power then there is no significant p-value less than 1%
mdf6.fit = lm(medv~poly(lstat,6), data=Boston)
summary(mdf6.fit)

#Call:
#  lm(formula = medv ~ poly(lstat, 6), data = Boston)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-14.7317  -3.1571  -0.6941   2.0756  26.8994 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       22.5328     0.2317  97.252  < 2e-16 ***
#  poly(lstat, 6)1 -152.4595     5.2119 -29.252  < 2e-16 ***
#  poly(lstat, 6)2   64.2272     5.2119  12.323  < 2e-16 ***
#  poly(lstat, 6)3  -27.0511     5.2119  -5.190 3.06e-07 ***
#  poly(lstat, 6)4   25.4517     5.2119   4.883 1.41e-06 ***
#  poly(lstat, 6)5  -19.2524     5.2119  -3.694 0.000245 ***
#  poly(lstat, 6)6    6.5088     5.2119   1.249 0.212313    <-----------here it's over 1% so not worth going to 6th power
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 5.212 on 499 degrees of freedom
#Multiple R-squared:  0.6827,	Adjusted R-squared:  0.6789 
#F-statistic: 178.9 on 6 and 499 DF,  p-value: < 2.2e-16

#try log()
mdf7.fit = lm(medv~log(lstat), data=Boston)
summary(mdf7.fit)
#Call:
#  lm(formula = medv ~ log(lstat), data = Boston)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-14.4599  -3.5006  -0.6686   2.1688  26.0129 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  52.1248     0.9652   54.00   <2e-16 ***
#  log(lstat)  -12.4810     0.3946  -31.63   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 5.329 on 504 degrees of freedom
#Multiple R-squared:  0.6649,	Adjusted R-squared:  0.6643 
#F-statistic:  1000 on 1 and 504 DF,  p-value: < 2.2e-16
anova(lm.fit, mdf2.fit, mdf3.fit, mdf4.fit, mdf6.fit, mdf7.fit)
#Analysis of Variance Table

#Model 1: medv ~ lstat
#Model 2: medv ~ (crim + zn + indus + chas + nox + rm + age + dis + rad + 
#                   tax + ptratio + black + lstat) - age - indus
#Model 3: medv ~ lstat + I(lstat^2)
#Model 4: medv ~ poly(lstat, 5)
#Model 5: medv ~ poly(lstat, 6)
#Model 6: medv ~ log(lstat)
#     Res.Df   RSS    Df     Sum of Sq       F    Pr(>F)    
#1    504     19472                                   
#2    494     11081   10    8391.0        37.4066 < 2.2e-16 *** <--- Rank1 - lower RSS is better
#3    503     15347   -9   -4265.9        21.1300 < 2.2e-16 ***
#4    500     13597    3    1750.2        26.0077 1.235e-15 *** <--- Rank 2 
#5    499     13555    1      42.4         1.8886      0.17    <---is is not significant, low F score means 1.8 std devations away from 0 null hypothesis
#6    504 1   4312    -5    -757.6          6.7545 4.187e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#--------------------------
# Qualitative Predictors
# aim to predict Sales (Child car seat sales) in 400 locations based on several predictors
#--------------------------

detach("package:ISLR", unload=TRUE)
install.packages("ISLR")
library(ISLR)
ls("package:ISLR")

fix(Carseats)
summary(Carseats)
names(Carseats)

#Sales - Unit sales (in thousands) at each location
#CompPrice - Price charged by competitor at each location
#Income -Community income level (in thousands of dollars)
#Advertising -Local advertising budget for company at each location (in thousands of dollars)
#Population-Population size in region (in thousands)
#Price-Price company charges for car seats at each site
#ShelveLoc- A factor with levels Bad, Good and Medium indicating the quality of the shelving location for the car seats at each site
#Age-Average age of the local population
#Education-Education level at each location
#Urban-A factor with levels No and Yes to indicate whether the store is in an urban or rural location
#US-A factor with levels No and Yes to indicate whether the store is in the US or not

pairs(Carseats)

car.fit=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats) #<-- note Price:Age is the same as Price*Age
summary(car.fit)

#Call:
#  lm(formula = Sales ~ . + Income:Advertising + Price:Age, data = Carseats)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.9208 -0.7503  0.0177  0.6754  3.3413 

#Coefficients:
#                       Estimate  Std. Error  t value   Pr(>|t|)    
#(Intercept)           6.5755654  1.0087470   6.519     2.22e-10 ***
#  CompPrice           0.0929371  0.0041183  22.567     < 2e-16 ***
#  Income              0.0108940  0.0026044   4.183     3.57e-05 ***
#  Advertising         0.0702462  0.0226091   3.107     0.002030 ** 
#  Population          0.0001592  0.0003679   0.433     0.665330    
#  Price              -0.1008064  0.0074399 -13.549     < 2e-16 ***
#  ShelveLocGood       4.8486762  0.1528378  31.724     < 2e-16 ***    #<---NOTE that the fact that the coefficent shelveLocGood is +ve indicates that a good shelf location is associated with high sales
#  ShelveLocMedium     1.9532620  0.1257682  15.531     < 2e-16 ***
#  Age                -0.0579466  0.0159506  -3.633     0.000318 ***
#  Education          -0.0208525  0.0196131  -1.063     0.288361    
#  UrbanYes            0.1401597  0.1124019   1.247     0.213171    
#  USYes              -0.1575571  0.1489234  -1.058     0.290729    
#  Income:Advertising  0.0007510  0.0002784   2.698     0.007290 ** 
#  Price:Age           0.0001068  0.0001333   0.801     0.423812    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.011 on 386 degrees of freedom
#Multiple R-squared:  0.8761,	Adjusted R-squared:  0.8719 
#F-statistic:   210 on 13 and 386 DF,  p-value: < 2.2e-16

attach(Carseats)
contrasts(ShelveLoc) #shows how R codes the categories for ShelveLoc

#---------
# exercise 8 
#---------
attach(Auto)
names(Auto)
fix(Auto)
auto.fit = lm(mpg~horsepower, data=Auto)
summary(auto.fit)
cor(mpg, horsepower)
plot(horsepower, mpg)
abline(auto.fit)

mean(mpg) #=23.44

#Call:
#  lm(formula = mpg ~ horsepower, data = Auto)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-13.5710  -3.2592  -0.3435   2.7630  16.9240 

#Coefficients:
#                   Estimate    Std. Error t value  Pr(>|t|)    
#(Intercept)        39.935861   0.717499    55.66    <2e-16 ***
#  horsepower       -0.157845   0.006446    -24.49   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 4.906 on 390 degrees of freedom
#Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
#F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16

#is there a relationship? 
#   yes, the p-value is less than close to 0

#how strong is the relationship?
#   R^2 is 0.6059 so there is a good relationship
#   RSE = 4.906, mean(mpg) = 23.44, this means that mpg deviates from mean by 4.906mpg (not bad) ie. 4.906/23.44 (20%)

#is the relationship -ve or +ve?
#   negative

#what is the predicted mpg associated with a horsepower of 98
predict(auto.fit, data.frame(horsepower=c(98)), interval="confidence")
#   fit      lwr      upr
#1  24.46708 23.97308 24.96108
predict(auto.fit, data.frame(horsepower=c(98)), interval="prediction")
#   fit      lwr      upr
#1  24.46708 14.8094 34.12476


#---------
# exercise 9 
#---------
pairs(Auto)
autononames = Auto[,c("mpg","cylinders","displacement","horsepower","weight","acceleration","year","origin")]
cor(autononames)
autononames.fit = lm(mpg~., data=autononames)
summary(autononames.fit)
plot(autononames.fit)
mean(mpg)

#Call:
#  lm(formula = mpg ~ ., data = autononames)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9.5903 -2.1565 -0.1169  1.8690 13.0604 

#Coefficients:
#                 Estimate  Std. Error t value Pr(>|t|)    
#  (Intercept)  -17.218435   4.644294  -3.707  0.00024 ***    
#  cylinders     -0.493376   0.323282  -1.526  0.12780    
#  displacement   0.019896   0.007515   2.647  0.00844 ** 
#  horsepower    -0.016951   0.013787  -1.230  0.21963    
#  weight        -0.006474   0.000652  -9.929  < 2e-16 ***   <--statistically significant
#  acceleration   0.080576   0.098845   0.815  0.41548    
#  year           0.750773   0.050973  14.729  < 2e-16 ***   <--statistically significant
#  origin         1.426141   0.278136   5.127 4.67e-07 ***   <--statistically significant
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 3.328 on 384 degrees of freedom
#Multiple R-squared:  0.8215,	Adjusted R-squared:  0.8182 
#F-statistic: 252.4 on 7 and 384 DF,  p-value: < 2.2e-16

#is there a relationship? 
#   yes, the p-value is less than close to 0
#   and R-squared value is close to 1

#how strong is the relationship?
#   R^2 is 0.8215 so there is a good relationship
#   RSE = 3.328, mean(mpg) = 23.44, this means that mpg deviates from mean by 3.328mpg (not bad) ie. 3.328/23.44 (14%)

#any highly leverages observations
#leverage statistics can be computed for any number of predictors using hatvalues()
plot(hatvalues(autononames.fit))
#tells you which obervation has the largest leverage statistic ie. 375
maxlstat = which.max(hatvalues(autononames.fit)) #ie. row 14
Auto[maxlstat,] #returns row
#you can also tag the extreme observation via identify
myval = identify(hatvalues(autononames.fit))
#   mpg cylinders displacement horsepower weight acceleration year origin                    name
#14  14         8          455        225   3086           10   70      1 buick estate wagon (sw)


autononames2.fit = lm(mpg~.+horsepower:weight+horsepower:year, data=autononames)
summary(autononames2.fit)
#Call:
#  lm(formula = mpg ~ . + horsepower:weight + horsepower:year, data = autononames)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8.1810 -1.4655 -0.1081  1.3735 11.2889 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         -5.334e+01  1.084e+01  -4.923 1.27e-06 ***
#  cylinders          2.444e-01  2.813e-01   0.869 0.385429    
#  displacement      -6.193e-04  6.595e-03  -0.094 0.925227    
#  horsepower         3.493e-01  1.050e-01   3.325 0.000969 ***
#  weight            -9.432e-03  7.682e-04 -12.278  < 2e-16 ***
#  acceleration      -1.248e-01  8.539e-02  -1.461 0.144731    
#  year               1.466e+00  1.304e-01  11.244  < 2e-16 ***
#  origin             7.995e-01  2.418e-01   3.307 0.001033 ** 
#  horsepower:weight  4.348e-05  5.443e-06   7.988 1.64e-14 ***
#  horsepower:year   -7.396e-03  1.306e-03  -5.662 2.95e-08 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2.819 on 382 degrees of freedom
#Multiple R-squared:  0.8725,	Adjusted R-squared:  0.8695 
#F-statistic: 290.5 on 9 and 382 DF,  p-value: < 2.2e-16
