setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Bank/knn/")
source("./../bank_import_primaryExplore.R")

library(dplyr)
library(class)
library(fastDummies)

# Method 2 - used scale inbulit R method to scale the method at place of normalization using z score
bank_n <- as.data.frame(scale(bank[c(1,11,12,13,14,16,17,18,19,20)]))
summary(bank_n)

source("bank_knn_sample_train.R")
