setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/3_PredictionModelEvaluation/Machine Learning with R/")

credit <- read.csv("credit.csv")

# estimating future performance of the model

#Holdout method using random sampling
# creating random 1000 variables and assiging ranks using runif and order function respectively
random_ids <- order(runif(1000)) # runif creates 1000 variables between 0 and 1
# creating train, test & validate datasets
credit_train <- credit[random_ids[1:500],]
credit_test <- credit[random_ids[501:750],]
credit_validate <- credit[random_ids[751:1000],]

# to eliminate biasing we use stratified random sampling using createDataPartition funtion from caret package
library(caret)
in_train <- caret::createDataPartition(y = credit$default, p = 0.75, list = FALSE)
credit_train.strat <- credit[in_train,]
credit_test.strat <- credit[-in_train,]

# K-Fold Cross-Validation 
# creating folds with k number of folds
folds <- caret::createFolds(y = credit$default, k = 10)

# manually setting the training and test datasets
credit01_test <- credit[folds$Fold01,]
credit01_trains <- credit[-folds$Fold01,]

# automatically checking the kappa values for all the folds to evaluate the model
set.seed(123)
# using lapply funtion to apply method for all the items in the list
cv_results <- lapply(folds, function(x){
  credit_test <- credit[x,]
  credit_train <- credit[-x,]
  credit_model <- C50::C5.0(default ~ . ,data = credit_train )
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit$default
  kappa <- irr::kappa2(ratings = data.frame(credit_actual, credit_pred))$value
  return(kappa)
})
mean(unlist(cv_results))






