
###############################################################################
## Lecture 2 Exercise 4
#  Principal Components Analysis and Regression
###############################################################################

# In this exercise we are going to run a Principal Components Analysis (PCA), 
# and train a Principal Components Regression (PCR)
# We will use two new data sets, USArrests and Hitters

library(ISLR)
# we will use the pls package for Principal Components Regression
install.packages("pls")
library(pls)

###########################################
# Principal Components Analysis exercise
###########################################

# We will look at the USArrests data set
head(USArrests)
# prcomp is a built in function that does a PCA 
# have a look at the documentation
?prcomp
pr_out = prcomp(USArrests, scale = T)
# The model has 5 output objects
names(pr_out)
# The 'rotation' output shows a view of the 'coefficients' of the principal components 
pr_out$rotation
# biplot outputs a plot of the first two principal components with the variables plotted

biplot(pr_out, scale=0)
 
# We will now create the scree plot by converting the standard deviation to a variance
pr_var = pr_out$sdev ^ 2
pve = pr_var / sum(pr_var)
# Scree plot
plot(pve, type = "b", main = "Scree Plot", 
     ylab = "Proportion of Variance Explained",
     xlab = "Principal Component")
# Plot of cumulative variance explained
plot(cumsum(pve), type = 'b', main = "Cumulative Variance Explained",
     xlab = "Cumulative Proportion of Variance Explained",
     ylab = "Number of Components")


###########################################
# Principal Components Regression exercise
###########################################

# The pcr function in the pls package does a Principal Components Regression
# Have a look at the documentation
?pcr
set.seed(1)
# train the model
pcr_fit = pcr(Salary ~ ., data = Hitters, scale = T, validation = "CV")
# The summary gives the % of variance explained, and a variation of MSE
summary(pcr_fit)
# Scree plot
validationplot(pcr_fit, val.type = "MSEP")

# To choose the number of components, we analyse the validation plot, 
# as well as the % variance explained in summary(pcr.fit)

# choose a number for the number of components
ncomp = 2 #<-based on plot looks two components look like it covers most variance.

# output predictions
pcr_pred = predict(pcr_fit, Hitters, ncomp = ncomp)

# Have a look at the relationship between predicted and actual
plot(Hitters$Salary, pcr_pred)

