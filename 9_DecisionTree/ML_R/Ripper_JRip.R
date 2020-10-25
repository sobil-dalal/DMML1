setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/9_DecisionTree/ML_R")
remove(list = ls())

library(RWeka)
library(caret)

# Step 1 : Collecting data
mushrooms <- read.csv("mushrooms.csv")

# Step 2 : Exploring and preparing the data
str(mushrooms)
summary(mushrooms)
mushrooms$veil_type <- NULL
table(mushrooms$type)

# Step 3 : Training a model on the data
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

# Step 4 : Evaluating model performance
summary(mushroom_1R)

# Step 5 : Improving model performance
# RIPPER
mushroom_JRip <- JRip(type ~ . , data = mushrooms)
mushroom_JRip

summary(mushroom_JRip)
