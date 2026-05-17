# =============================================================================
# 03 — Regressions and predictions
# Accompanies: slides/03_regression_analysis.pdf
# Required packages: tidyverse, stargazer, car, caTools, caret, MASS
# =============================================================================

rm(list = ls())
cat("\014")

### Libraries ---------------------------------------------------------------

library(tidyverse)
library(stargazer)
library(car)
library(caTools)
library(caret)

### Load Data ---------------------------------------------------------------
setwd("./")

# This is where we load the dataset. You will need to set the name to the name of the file you need to load which should be in the working directory.
df <- read.csv("output/Cleaned_Dataset_Mittelsachsen_Kreis.csv")
df$X <- NULL
df$heatingType<-as.factor(df$heatingType)



### Training Linear Regressions -----------------------------------------

#Linear regressions assume a linear relationship between your independent variables and the dependent variable (but NOT between the independent variables themselves)

# As in lecture 8, we investigate the (linearity of) relationships between two metric variables using scatterplots.

# You can quickly and easily create scatter plots between variables by using the function
# "pairs()". The function creates scatter plots for all variables contained in the dataframe
# you use as an input. 
# Please note that even though it is possible to create a scatter plot for categorical variables
# as well, the number of insights you are able to derive from those plots is fairly limited. However,
# using bubble charts or jittering, this is also possible to some extent. For the regression analysis, we focus on metric variables.
names(df)
df.numeric.variables <- df %>%
  select(totalRent,yearConstructed,livingSpace)

####Splitting the data in training and testing data. We estimate all models with the training data
set.seed(815) # so we all have the same datasets

sample = sample.split(df.numeric.variables, SplitRatio = .8)

train = subset(df.numeric.variables, sample == TRUE)
test = subset(df.numeric.variables, sample == FALSE)



## Scatter plot
# Even though you already created a scatter plot for the dataset, there are occasions on which you
# may want to take a closer look at a specific scatter plot. For that ggplot also provides a function.
# I assume that most of you know scatter plots and how they are used. I will therefore
# not describe them in much detail. 
# The code below plots the variable "livingSpace" against "totalRent". 
scatter <- ggplot(train, aes(x=livingSpace, y= totalRent)) +
  geom_point() + 
  ggtitle("Scatter Plot of Rent vs Living Space")
scatter

# Now we look into all the relationships.
pairs(train)
# The plots show a clear linear relationship between living space and rent. However, it is far less clear for the year constructed.

#Next, we check the pearson correlations for those relationships.
cor(train)

##This confirms what we saw in the diagram. total rent and living space are highly correlated.
# There is only a small negative correlation between yearConstructed and totalRent however. And living space increases with age.

###Now we estimate a simple OLS REGRESSION model with our more promising predictor: livingSpace
model1 <- lm(totalRent ~ livingSpace , data = train)

##This estimated an ols model and stores it, along with additional information in the object model1. 

##Exercise
model2 <- lm(totalRent ~ yearConstructed , data = train)



# You can get a summary with the summary() command

summary(model1)
summary(model2)

# Now we estimate a multivariate model with two predictors. 
model3 <- lm(totalRent ~ livingSpace + yearConstructed, data = df)
# Caveat: model3 is fit on the full dataset `df`, whereas model1/model2 use `train`.
# The held-out evaluation below therefore overlaps with model3's training rows.
# For a strictly out-of-sample test, refit model3 with data = train.
summary(model3)
stargazer(model3,type = "text")

# We can get a nicer display of the results using the stargazer package
stargazer(model1,model2,model3, type = "text")


# Predicting for test data and storing predictions as new variable in test data frame
test$prediction <- predict(model3, newdata=test)
test$prediction

# Calculating residuals
test <- test %>%
  mutate(residuals = prediction - totalRent)

# Calculating MAPE
test <- test %>%
  mutate(MAPE = abs(prediction - totalRent)/totalRent)
mean(test$MAPE)

# Calculating Sales Ratios
test <- test %>%
  mutate(SR = prediction / totalRent)
mean(test$SR)


### Evaluating Generalisation Performance ----------------------------------------------

# Define number of folds
k <- 3

# Create folds
folds <- createFolds(df.numeric.variables$totalRent, k = 3, list = FALSE)
folds

# Train a model on (k-1) folds and keep 1 fold for testing
for(i in 1:k){
  # We keep fold i for testing the model and use all other folds for training the model.
  test <- df.numeric.variables%>%
    filter(folds==i)
  # print(length(df.testing$totalRent))
  
  train <- df.numeric.variables%>%
    filter(folds!=i)
  
  # Training model
  model <- lm(totalRent~livingSpace+yearConstructed,data=train)
  
  # Predict using the model and testing data
  test$prediction <- predict(model, newdata=test)
  
  # Calculating MAPE
  test <- test %>%
    mutate(MAPE = abs(prediction - totalRent)/totalRent)
  
  # Calculating Sales Ratios
  test <- test %>%
    mutate(SR = prediction / totalRent)
  
  
  cat("Model summary for fold",i,":\n")
  print(summary(model))
  
  cat("Mean Average Percentage Error for fold",i,":\n")
  print(mean(test$MAPE))
  
  cat("Sales Ratio for fold",i,":\n")
  print(mean(test$SR))
  
  cat("\n")
  cat("\n")
  cat("-----------------------")
  cat("\n")
  cat("\n")
}

### Testing Assumptions -----------------------------------------------------
# See also: https://www.r-bloggers.com/regression-diagnostics-with-r/

## Variable Class and Linearity
scatter <- ggplot(df.numeric.variables, aes(x=totalRent, y=livingSpace)) +
  geom_point() + 
  ggtitle("Scatter Plot of Rent vs Living Space")
scatter

## Independence between error terms and predictors
test <- test %>%
  mutate(residuals = totalRent - prediction)
cor(test)

# Caveat: the chisq.test below is not a valid test of error/predictor independence
# (it runs a goodness-of-fit test on three concatenated numeric vectors).
# The residual/predictor correlations from cor(test) above are the relevant check.
library(MASS)
test$residuals.temp <- test$residuals + 145
tbl <- c(test$residuals.temp,test$yearConstructed,test$livingSpace)
chisq.test(tbl)

## Multicollinearity -> Variance Inflation Factors; values near 1 are great; 
# In textbooks, cutoffs for problematic VIFs are given at 4, 8 or 10...
vif(model3)


## Homoscedasticity
scatter <- ggplot(test, aes(x=prediction, y=residuals)) +
  geom_point() + 
  ggtitle("Scatter Plot of Residuals vs Fitted Values")
scatter
# We can see a clear fan-like spread. This indicates that the variability in
# the error terms increases, as the prediction increases ==> Probably not homoscedastic.
# https://www.youtube.com/watch?v=m8Lx_zGnmjU
# spreadLevelPlot(model3)


## Residuals Normally Distributed  
ggplot() + geom_density(aes(residuals(model3)))

## Non-zero Variance in the predictors
sd(test$livingSpace)
sd(test$yearConstructed)

### Robustness --------------------------------------------------------------

# Load messy dataset
df.messy <- read.csv("data/Dataset_Teachers.csv")
df.messy$X <- NULL
df.messy.Mittelsachsen <- df.messy %>%
  filter(regio2=="Mittelsachsen_Kreis")
df.messy.Mittelsachsen$heatingType<-as.factor(df.messy.Mittelsachsen$heatingType)

df.messy.numeric.variables <- df.messy.Mittelsachsen %>%
  select(totalRent,
         yearConstructed,
         livingSpace)

####Splitting the data in training and testing data. We estimate all models with the training data
set.seed(815) # so we all have the same datasets
sample = sample.split(df.messy.numeric.variables, SplitRatio = .8)
train.messy = subset(df.messy.numeric.variables, sample == TRUE)
test.messy = subset(df.messy.numeric.variables, sample == FALSE)

# Training models on messy dataset
model3.messy <- lm(totalRent ~ livingSpace + yearConstructed , data = train.messy)

# Comparing messy models
stargazer(model3,model3.messy, type = "text")

# Predicting for test data and storing predictions as new variable in test data frame
test.messy$prediction <- predict(model3.messy, newdata=test.messy)
test.messy$prediction

# Calculating MAPE & Sales Ratio
test.messy <- test.messy %>%
  filter(totalRent!=0) %>%
  mutate(MAPE = abs(prediction - totalRent)/totalRent)
test.messy <- test.messy %>%
  mutate(SR = prediction / totalRent)

cat("Mean Average Percentage Error for model trained on messy data:",mean(test.messy$MAPE,na.rm=T),"\n")
cat("Mean Average Percentage Error for model trained on clean data:",mean(test$MAPE),"\n")

cat("Sales Ratio for model trained on messy data:",mean(test.messy$SR,na.rm=T),"\n")
cat("Sales Ratio for model trained on clean data:",mean(test$SR,na.rm=T),"\n")



### Evaluating which observations have a strong impact on the model
# Cook's distance
summary(cooks.distance(model3.messy))
plot(model3.messy, which=4)

# Leverage
lev <- hatvalues(model3.messy)
plot(lev)

#Hat values twice the average are usually considered noteworthy
mean(lev)
lev[lev > (2*mean(lev))]


# .0 Archive ---------------------------------------------------------------

### k-fold CV for messy dataset
df.messy.numeric.variables <- na.omit(df.messy.numeric.variables)

# Define number of folds
k <- 3

# Create folds
folds <- createFolds(df.messy.numeric.variables$totalRent, k = 3, list = FALSE)
folds

# Train a model on (k-1) folds and keep 1 fold for testing
for(i in 1:k){
  # We keep fold i for testing the model and use all other folds for training the model.
  test.messy <- df.messy.numeric.variables%>%
    filter(folds==i)
  # print(length(df.testing$totalRent))
  
  train.messy <- df.messy.numeric.variables%>%
    filter(folds!=i)
  
  # Training model
  model <- lm(totalRent~livingSpace+yearConstructed,data=train.messy)
  
  # Predict using the model and testing data
  test.messy$prediction <- predict(model, newdata=test.messy)
  
  # Calculating MAPE
  test.messy <- test.messy %>%
    mutate(MAPE = abs(prediction - totalRent)/totalRent)
  
  # Calculating Sales Ratios
  test.messy <- test.messy %>%
    mutate(SR = prediction / totalRent)

  
  cat("Model summary for fold",i,":\n")
  print(summary(model))
  
  cat("Mean Average Percentage Error for fold",i,":\n")
  print(mean(test.messy$MAPE))
  
  cat("Sales Ratio for fold",i,":\n")
  print(mean(test.messy$SR))
  
  cat("\n")
  cat("\n")
  cat("-----------------------")
  cat("\n")
  cat("\n")
}


#####Influential Observations and Outliers

##Outliers. Testing Outliers.
# Reports the Bonferroni p-values for testing each observation in turn to be a mean-shift outlier, based on Studentized residuals in linear (t-tests).
outlierTest(model3)

##Cook's distance as a measure of the influence of a single case on the model as a whole. >1 may be a problem
summary(cooks.distance(model3))
plot(model1, which=4)


##leverage
lev <- hatvalues(model3)
plot(lev)

#Hat values twice the average are usually considered noteworthy
mean(lev)
lev[lev>(2*mean(lev))]

## Linearity
plot(model3, which = 1)