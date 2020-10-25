setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/9_DecisionTree/ML_R")
remove(list = ls())

library(C50)
library(caret)

# Step 1 : Collecting data
credit <- read.csv("credit.csv")

# Step 2 : Exploring and preparing the data
str(credit)
summary(credit)
table(credit$default)

# Data preparation
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)

# creating train & test data sets
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Step 3 : Training a model on the data
credit_model <- C50::C5.0(credit_train[-17], credit_train$default)
credit_model

summary(credit_model)

# Step 4 : Evaluating model performance
credit_predict <- predict(object = credit_model, newdata = credit_test)

caret::confusionMatrix(credit_predict, credit_test$default)

# Step 5 : Improving model performance
credit_boost10 <- C50::C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10

summary(credit_boost10)
credit_boost_predict10 <- predict(object = credit_boost10, newdata = credit_test)
caret::confusionMatrix(credit_boost_predict10, credit_test$default)


# Adding mistakes more costlier than others
mtr_dim <- list(c("no", "yes"), c("no","yes"))
names(mtr_dim) <- c("predict","actual")
mtr_dim

err_cst <- matrix(c(0,1,0,4), nrow = 2, dimnames = mtr_dim)
err_cst


# Model Costs
credit_cost <- C50::C5.0(credit_train[-17], credit_train$default, costs = err_cst)
credit_cost_predict <- predict(object = credit_cost, newdata = credit_test)
caret::confusionMatrix(credit_cost_predict, credit_test$default)
