setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Bank/Decision_tree/")
source("./../bank_import_primaryExplore.R")

library(C50)
library(caret)
library(RWeka)

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
bank_1R <- OneR(y ~ ., data = bank_train)
bank_1R
summary(bank_1R)

# Step 4 : Evaluating model performance
bank_predict_1R <- predict(object = bank_1R, newdata = bank_test)
caret::confusionMatrix(bank_predict_1R, bank_test$y)
# kappa = 0.2576,   Sensitivity = 0.21121,    Specificity = 0.97756 

# Step 5 : Improving model performance
# RIPPER
bank_JRip <- JRip(y ~ . , data = bank_train)
bank_JRip
summary(bank_JRip)

bank_predict_JRip <- predict(object = bank_JRip, newdata = bank_test)
caret::confusionMatrix(bank_predict_JRip, bank_test$y)
# kappa = 0.5667,   Sensitivity = 0.58513,    Specificity = 0.95895 ====> BEST