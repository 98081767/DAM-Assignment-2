
###############################################################################
## Lecture 2 Exercise 3
#  Ridge Regression and Lasso
###############################################################################

# In this exercise we will be training a ridge regression and the lasso
# We will use the same OJ data set as in exercise 2

# Ridge regression and lasso can be trained using the glmnet package
install.packages("glmnet")
library(glmnet)
library(caret)
install.packages("ISLR")
library(ISLR)

# recall the OJ data set for classification modelling.  Purchase is our target
# create data partition row list
set.seed(42)
train = createDataPartition(y = OJ$Purchase, p = 0.7, list = F)
# partition default data - remove the variable Store7
training = OJ[train, ]
testing = OJ[-train, ]

# The glmnet package requires data be in separate x and y sets.  
# the predictor set, x, must be a matrix
x = model.matrix(~ ., training[, -1])
y = training$Purchase

# We will use the cv.glmnet function to train our model
# This function automatically selects the optimal lambda
# Have a look at the documentation,
?cv.glmnet

# Setting the alpha argument to 0 does ridge regression, 1 does lasso

###########################
# Ridge Regression

set.seed(42)
  # alpha = 0 specifies ridge regression
cv.fit_ridge = cv.glmnet(x, y, family = 'binomial', alpha = 0)

# Results
plot(cv.fit_ridge)
cv.fit_ridge$lambda.min
cv.fit_ridge$lambda.1se
#shows the coefficents
coef(cv.fit_ridge, s = cv.fit_ridge$lambda.min)

prediction_ridge = predict(cv.fit_ridge$glmnet.fit, newx = model.matrix(~ ., testing[, -1]), 
                          type = "class",
                          s = cv.fit_ridge$lambda.min)


ridge_confusion = confusionMatrix(data = as.factor(prediction_ridge), testing$Purchase)
ridge_confusion

###########################
# Lasso Regression

set.seed(42)
# alpha = 1 specifies lasso regression
cv.fit_lasso = cv.glmnet(x, y, family = 'binomial', alpha = 1)

# Results
plot(cv.fit_lasso)
cv.fit_lasso$lambda.min #error measure. can choose either
cv.fit_lasso$lambda.1se #error measure. can choose either
coef(cv.fit_lasso, s = cv.fit_lasso$lambda.min)

prediction_lasso = predict(cv.fit_lasso$glmnet.fit, newx = model.matrix(~ ., testing[, -1]), 
                     type = "class",
                     s = cv.fit_lasso$lambda.min)

lasso_confusion = confusionMatrix(data = as.factor(prediction_lasso), testing$Purchase)

###########################
# Evaluate the models

ridge_confusion
lasso_confusion



