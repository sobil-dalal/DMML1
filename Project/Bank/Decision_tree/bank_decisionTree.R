setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Bank/Decision_tree/")
source("./../bank_import_primaryExplore.R")

library(C50)
library(caret)

# Step 1 : Collecting data
# Done in source above

# Step 2 : Exploring and preparing the data
# Done in source above

# creating training and testing dataset from exisitng sample
indx <- createDataPartition(bank$y, p = 0.8, list = FALSE)

bank_train <- bank[indx,]
bank_test <- bank[- indx,]

prop.table(table(bank_train$y))
prop.table(table(bank_test$y))

# Step 3 : Training a model on the data
bank_model <- C50::C5.0(bank_train[-21],bank_train$y)
bank_model

summary(bank_model)

# Step 4 : Evaluating model performance
bank_predict <- predict(object = bank_model, newdata = bank_test)

caret::confusionMatrix(bank_predict, bank_test$y)
# kappa = 0.5623,   Sensitivity = 0.55927,    Specificity = 0.96415 ===> BEST




# Step 5 : Improving model performance
# Method 1 - 
bank_boost10 <- C50::C5.0(bank_train[-21], bank_train$y, trials = 10)
bank_boost10

summary(bank_boost10)
bank_boost_predict10 <- predict(object = bank_boost10, newdata = bank_test)
caret::confusionMatrix(bank_boost_predict10, bank_test$y)
# kappa = 0.5438,   Sensitivity = 0.54957,    Specificity = 0.96073 

# Method 2 - 
bank_boost5 <- C50::C5.0(bank_train[-21], bank_train$y, trials = 5)
bank_boost5

summary(bank_boost5)
bank_boost_predict5 <- predict(object = bank_boost5, newdata = bank_test)
caret::confusionMatrix(bank_boost_predict5, bank_test$y)
# kappa = 0.5459,   Sensitivity = 0.56142,    Specificity = 0.95827 


# Method 3 -
# Adding mistakes more costlier than others
mtr_dim <- list(c("no", "yes"), c("no","yes"))
names(mtr_dim) <- c("predict","actual")
mtr_dim

err_cst <- matrix(c(0,1,0,4), nrow = 2, dimnames = mtr_dim)
err_cst

# Model Costs
bank_cost <- C50::C5.0(bank_train[-21], bank_train$y, costs = err_cst)
bank_cost_predict <- predict(object = bank_cost, newdata = bank_test)
caret::confusionMatrix(bank_cost_predict, bank_test$y)
# kappa = 0.5623,   Sensitivity = 0.55927,    Specificity = 0.96415