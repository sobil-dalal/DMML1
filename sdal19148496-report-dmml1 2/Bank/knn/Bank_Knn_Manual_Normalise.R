setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Bank/knn/")
source("./../bank_import_primaryExplore.R")

library(dplyr)
library(class)
library(fastDummies)

# normalizing the data to standardize range of values of various characterstics
# creating normalized method 
nor <- function(val) {
  return((val - min(val))/(max(val) - min(val)))
}

# getting normalised variables in a data frame
bank_n_cont <- as.data.frame(lapply(bank[c(1,11,12,13,14,16,17,18,19,20)], nor))
summary(bank_n_cont)

# creating a new df of only independent variables
bank_n <- bank[,-21]
bank_n$age <- bank_n_cont$age
bank_n$duration <- bank_n_cont$duration
bank_n$campaign <- bank_n_cont$campaign
bank_n$pdays <- bank_n_cont$pdays
bank_n$previous <- bank_n_cont$previous
bank_n$emp.var.rate <- bank_n_cont$emp.var.rate
bank_n$cons.price.idx <- bank_n_cont$cons.price.idx
bank_n$cons.conf.idx <- bank_n_cont$cons.conf.idx
bank_n$euribor3m <- bank_n_cont$euribor3m
bank_n$nr.employed <- bank_n_cont$nr.employed
summary(bank_n)

# creating dummy columns for categorical columns
bank_n <- fastDummies::dummy_cols(bank_n, remove_first_dummy = TRUE) # remove_first_dummy = TRUE to avoid multi-collinearity

# removing categorical columns from df (dummy are included)
bank_n <- bank_n[,-c(2:10,15)]

source("bank_knn_sample_train.R")