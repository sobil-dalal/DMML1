remove(list = ls())
setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Bank/")

set.seed(1)
library(gmodels)

# STEP 1 - COLLECTING DATA AND CHECKING THE COLUMNS/ FACTORS
# reading the file
bank <- read.csv("bank-additional-full.csv", sep = ";")



# STEP 2 - EXPLORING AND PREPARING THE DATA
str(bank)

# checking the overall distribution of prediction/ target charcterstics/feature
gmodels::CrossTable(bank$y)

# converting the target feature into factor with adding labels for more information
bank$y <- factor(bank$y, levels = c("yes", "no"), labels = c("Yes", "No"))
# cheking NAs
apply(X = bank,MARGIN =  2, FUN = function(col) any(is.na(col))) # no NAs

# checking the summary of bank
summary(bank)





