setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Bank/SVM/")
source("./../bank_import_primaryExplore.R")

library(caret)
library(kernlab)

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
bank_svm <- ksvm(y ~ ., data = bank_train, kernel = "vanilladot")
bank_svm



# Step 4 : Evaluating model performance
bank_predict_svm <- predict(object = bank_svm, newdata = bank_test)
caret::confusionMatrix(bank_predict_svm, bank_test$y)
# kappa = 0.4051,   Sensitivity = 0.33297,    Specificity = 0.98085



# Step 5 : Improving model performance
# Method 1 : kernel = rbfdot
bank_svm.rbf <- ksvm(y ~ . , data = bank_train, kernel = "rbfdot")

bank_predict_svm.rbf <- predict(object = bank_svm.rbf, newdata = bank_test)
caret::confusionMatrix(bank_predict_svm.rbf, bank_test$y)
# kappa = 0.4907,   Sensitivity = 0.42996,    Specificity = 0.97715 

# Method 2 : kernel = polydot
bank_svm.poly <- ksvm(y ~ . , data = bank_train, kernel = "polydot")

bank_predict_svm.poly <- predict(object = bank_svm.poly, newdata = bank_test)
caret::confusionMatrix(bank_predict_svm.poly, bank_test$y)
# kappa = 0.4051,   Sensitivity = 0.33297,    Specificity = 0.98085 

# Method 3 : kernel = tanhdot
bank_svm.tanh <- ksvm(y ~ . , data = bank_train, kernel = "tanhdot")

bank_predict_svm.tanh <- predict(object = bank_svm.tanh, newdata = bank_test)
caret::confusionMatrix(bank_predict_svm.tanh, bank_test$y)
# kappa = 0.17,   Sensitivity = 0.26078,    Specificity = 0.90806



# from above 3 methods - trying adding cost param in kernel = rbfdot
# Method 4 : kernel = rbfdot & C = 10
bank_svm.rbf10 <- ksvm(y ~ . , data = bank_train, kernel = "rbfdot", C = 10)

bank_predict_svm.rbf10 <- predict(object = bank_svm.rbf10, newdata = bank_test)
caret::confusionMatrix(bank_predict_svm.rbf10, bank_test$y)
# kappa = 0.4991,   Sensitivity = 0.47737,    Specificity = 0.96607 

# Method 5 : kernel = rbfdot & C = 50
bank_svm.rbf50 <- ksvm(y ~ . , data = bank_train, kernel = "rbfdot", C = 50)

bank_predict_svm.rbf50 <- predict(object = bank_svm.rbf50, newdata = bank_test)
caret::confusionMatrix(bank_predict_svm.rbf50, bank_test$y)
# kappa = 0.4848,   Sensitivity = 0.49461,    Specificity = 0.95622

# Method 6 : kernel = rbfdot & C = 100
bank_svm.rbf100 <- ksvm(y ~ . , data = bank_train, kernel = "rbfdot", C = 100)

bank_predict_svm.rbf100 <- predict(object = bank_svm.rbf100, newdata = bank_test)
caret::confusionMatrix(bank_predict_svm.rbf100, bank_test$y)
# kappa = 0.4853,   Sensitivity = 0.51509,    Specificity = 0.95034 ==> BEST