setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Kc_house/")
source("Kc_Import_Explore_Clean.R")

library(RWeka)
library(rpart)
library(rpart.plot)
library(caret)

set.seed(1)

# Step 1 : Collecting data
# Step 2 : Explore Data & data prepration
# Done above to steps in the source - Kc_Import_Explore_Clean.R script (except data prepration)

# creating training and testing dataset from exisitng sample
indx <- createDataPartition(kc_house$price, p = 0.8, list = FALSE)

house_train <- kc_house[indx,]
house_test <- kc_house[- indx,]

# Step 3 : Training a model
# regression tree
house.rpart <- rpart(price ~ ., data = house_train)
house.rpart
summary(house.rpart)

# visulazing decision tree
rpart.plot(house.rpart, digits = 3)

# Step 4 : Evaluating model performace
house.predict.rpart <- predict(house.rpart, house_test)
summary(house.predict.rpart)
summary(house_test$price)

# checking correlation 
cor(house.predict.rpart, house_test$price) # 0.7713

# cheking performace with MAE
mae <- function(actual, pred) {
  mean(abs(actual-pred))
}

mae(house_test$price, house.predict.rpart) # 158941.5
mae(mean(house_train$price), house.predict.rpart) # 187045.4



# Step 5 : Improving model
# model tree
house.m5p <- M5P(price ~ . , data = house_train)
house.m5p
summary(house.m5p)

house.predict.m5p <- predict(house.m5p, house_test)
summary(house.predict.m5p)
cor(house.predict.m5p, house_test$price) # 0.6551532
mae(house_test$price, house.predict.m5p) # 8756021
