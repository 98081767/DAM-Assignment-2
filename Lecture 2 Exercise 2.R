
###############################################################################
## Lecture 2 Exercise 2
#  Logistic Regression
###############################################################################

# This exercise involves training a logistic regression model
# We will us the OJ data set from the ISLR package - Orange Juice sales
# We will also use the caret package to make the process easier

install.packages('caret', dependencies = TRUE)
library(caret)

# Let's start by summarising our data
str(OJ)
summary(OJ)

###########################
# Paritioning
###########################

# We want to partition our data into 70% for training, 30% for testing

# create data partition row list
set.seed(42)  # setting a random seed ensures we get the same result each time
# We will use the function 'createDataPartition' from the caret package
?createDataPartition
train = createDataPartition(y = OJ$Purchase, p = 0.7, list = F)
# partition OJ data into two sets 
training = OJ[train, ]
testing = OJ[-train, ]
str(training)
str(testing)

###########################
# Variable selection
###########################

# In this section, we will select which variables we want to include in our model
# We'll do this by backwards selection - start with everything and remove one by one

# let's start by throwing all the variables into the logistic regression
OJ.glm = glm(formula = Purchase ~ .,
             data = training,
             family = "binomial")
summary(OJ.glm)
# AIC ~ 581.8

# It's clear that we can remove some variables. This should drop the AIC
OJ.glm = glm(formula = Purchase ~ WeekofPurchase + StoreID + PriceCH + PriceMM + DiscMM + SpecialCH + SpecialMM + LoyalCH + 
               PctDiscMM + PctDiscCH,
             data = training,
             family = "binomial")
summary(OJ.glm)
# AIC ~ 577.91

# We can probably remove a few more
OJ.glm = glm(formula = Purchase ~ StoreID + PriceCH + PriceMM + DiscMM + LoyalCH + PctDiscMM + PctDiscCH,
             data = training,
             family = "binomial")
summary(OJ.glm)
# AIC ~ 575

# Let's stick with this last model

# we may have multi-collinearity present.  Use the 'pairs' plot function to check
pairs(OJ[, c("StoreID","PriceCH","PriceMM","DiscMM","LoyalCH","PctDiscMM","PctDiscCH")])

# QUESTION - are any predictors collinear?
# yes - DiscMM and PctDiscMM

###########################
# Create probabilities and predictions
###########################

# add the probabilities to the testing data
testing$probability = predict(OJ.glm, newdata = testing, type = "response")

# assume that the optimum probability threshold is 0.5
# Create the class prediction - our target is the "MM" class
testing$prediction = "CH"
testing[testing$probability >= 0.5, "prediction"] = "MM"

# Have a look at the data
head(testing)

###########################
# Evaluation

# Create a confusion matrix (along with other measures) using the 
# function 'confusionMatrix' from the caret package
myPred = as.factor(testing$prediction)
str(myPred)
str(testing$prediction)
str(testing$Purchase)
testing$prediction
testing$Purchase

confusionMatrix(data = myPred, testing$Purchase)
#Confusion Matrix and Statistics

#Reference
#Prediction  CH  MM
# CH 169  33
# MM  26  92
# 
# Accuracy : 0.8156          
# 95% CI : (0.7687, 0.8566)
# No Information Rate : 0.6094          
# P-Value [Acc > NIR] : 1.373e-15       
# 
# Kappa : 0.6088          
# Mcnemar's Test P-Value : 0.4347          
# 
# Sensitivity : 0.8667          
# Specificity : 0.7360          
# Pos Pred Value : 0.8366          
# Neg Pred Value : 0.7797          
# Prevalence : 0.6094          
# Detection Rate : 0.5281          
# Detection Prevalence : 0.6312          
# Balanced Accuracy : 0.8013          
# 
# 'Positive' Class : CH              


