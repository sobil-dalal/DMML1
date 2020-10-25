library(caret)

# creating training and testing dataset from exisitng sample
indx <- createDataPartition(bank$y, p = 0.8, list = FALSE)

bank_train <- bank_n[indx,]
bank_test <- bank_n[- indx,]

# creating lables for test and training data sets
bank_train_labels <- bank[indx,21]
bank_test_labels <- bank[- indx,21]




# STEP 3 - TRAINING A MODEL ON THE DATA
# using the knn method of class package with K value equalent to square root of 
# total train observation & odd number to eliminate tie vote issue for 2 factor classification i.e. 
bank$y %>% length %>% sqrt %>% round # 203
bank_test_predict <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = 203)




# STEP 4 - EVALUATING MODEL PERFORMANCE
print("Model 1 : k = 203")
print(confusionMatrix(data = bank_test_predict,reference = bank_test_labels))
# Inbuilt ==>   kappa =  0.4108      ,     Sensitivity = 0.32866
# Manual Normlization ==>   kappa =  0.2307      ,     Sensitivity = 0.15409
# Only Numeric ==>   kappa =  0.4382      ,     Sensitivity = 0.36853

# STEP 5 - IMPROVING MODEL PERFORMANCE
#Method 2
# by changing k values
# predict = 2     : k = 151
bank_test_predict2 <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = 151)
print("Model 2 : k = 151")
print(confusionMatrix(data = bank_test_predict2,reference = bank_test_labels))
# Inbuilt ==>   kappa =  0.4307      ,     Sensitivity = 0.34698
# Manual Normlization ==>   kappa =  0.2292      ,     Sensitivity = 0.15517
# Only Numeric  ==>   kappa =  0.4491      ,     Sensitivity = 0.38362

# predict = 3     : k = 101
bank_test_predict3 <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = 101)
print("Model 3 : k = 101")
print(confusionMatrix(data = bank_test_predict3,reference = bank_test_labels))
# Inbuilt ==>   kappa =  0.4463     ,     Sensitivity = 0.36530
# Only Numeric  ==>   kappa =  0.4695      ,     Sensitivity = 0.41595

# predict = 4     : k = 75
bank_test_predict4 <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = 75)
print("Model 4 : k = 75")
print(confusionMatrix(data = bank_test_predict4,reference = bank_test_labels))
# Inbuilt ==>   kappa =  0.4546      ,     Sensitivity = 0.37823
# Only Numeric  ==>   kappa =  0.4823      ,     Sensitivity = 0.43319

# predict = 5     : k = 51
bank_test_predict5 <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = 51)
print("Model 5 : k = 51")
print(confusionMatrix(data = bank_test_predict5,reference = bank_test_labels))
# Inbuilt ==>   kappa =  0.4627      ,     Sensitivity = 0.39332
# Only Numeric  ==>   kappa =  0.4896      ,     Sensitivity = 0.44935

# predict = 6     : k = 31
bank_test_predict6 <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = 31)
print("Model 6 : k = 31")
print(confusionMatrix(data = bank_test_predict6,reference = bank_test_labels))
# Inbuilt ==>   kappa =  0.4689      ,     Sensitivity = 0.40409
# Only Numeric  ==>   kappa =  0.5196      ,     Sensitivity = 0.48276

# predict = 7     : k = 21
bank_test_predict7 <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = 21)
print("Model 7 : k = 21")
print(confusionMatrix(data = bank_test_predict7,reference = bank_test_labels))
# Inbuilt ==>   kappa =  0.4626      ,     Sensitivity = 0.41056
# Only Numeric  ==>   kappa =  0.5313      ,     Sensitivity = 0.50108

# predict = 8     : k = 11
bank_test_predict8 <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = 11)
print("Model 8 : k = 11")
print(confusionMatrix(data = bank_test_predict8,reference = bank_test_labels))
# Inbuilt ==>   kappa =  0.4561      ,     Sensitivity = 0.41918
# Manual Normlization ==>   kappa =  0.2977      ,     Sensitivity = 0.23384
# Only Numeric  ==>   kappa =  0.5049      ,     Sensitivity = 0.49461

print("Model n : k = 9 to 1 (only odd numbers)")
i <- 9
while (i > 0) {
  bank_test_predict_n <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = i)
  print(i)
  print(confusionMatrix(data = bank_test_predict_n,reference = bank_test_labels))
  i <- i - 2
}
#Best among above
# Inbuilt ==>   kappa =  0.4744      ,     Sensitivity = 0.44289,     Accuracy = 0.908  for K = 9     =============> BEST FOR INBUILT Normalization (scale)
# Manual Normlization ==>   kappa =  0.3052      ,     Sensitivity = 0.28125,   Accuracy = 0.8879 for k = 3 =============> BEST FOR Manual Normalization
# Only Numeric  ==>   kappa =  0.5068      ,     Sensitivity = 0.51509 for k = 5

print("Model n : k = 19 to 13 (only odd numbers)")
i <- 19
while (i > 11) {
  bank_test_predict_n <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = i)
  print(i)
  print(confusionMatrix(data = bank_test_predict_n,reference = bank_test_labels))
  i <- i - 2
}
#Best among above
# Inbuilt ==>   kappa =  0.4665      ,     Sensitivity = 0.42672  for K = 13
# Only Numeric  ==>   kappa =  0.532      ,     Sensitivity = 0.50539 for k = 19    =============> BEST FOR ONLY NUMERIC

print("Model n : k = 29 to 23 (only odd numbers)")
i <- 29
while (i > 21) {
  bank_test_predict_n <- class::knn(train = bank_train, test = bank_test, cl = bank_train_labels, k = i)
  print(i)
  print(confusionMatrix(data = bank_test_predict_n,reference = bank_test_labels))
  i <- i - 2
}
#Best among above
# Only Numeric  ==>   kappa =  0.5255      ,     Sensitivity = 0.49892 for k = 23








# BEST OF ALL - Only Numeric for K = 19
"
[1] 19
Confusion Matrix and Statistics

          Reference
Prediction  Yes   No
       Yes  469  226
       No   459 7083
                                          
               Accuracy : 0.9168          
                 95% CI : (0.9107, 0.9227)
    No Information Rate : 0.8873          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.5329          
                                          
 Mcnemar's Test P-Value : < 2.2e-16       
                                          
            Sensitivity : 0.50539         
            Specificity : 0.96908         
         Pos Pred Value : 0.67482         
         Neg Pred Value : 0.93914         
             Prevalence : 0.11266         
         Detection Rate : 0.05694         
   Detection Prevalence : 0.08438         
      Balanced Accuracy : 0.73723         
                                          
       'Positive' Class : Yes          
"


