remove(list = ls())
setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/4_RegressionModelsClassificationKNN/ML with R/")




# STEP 1 - COLLECTING DATA AND CHECKING THE COLUMNS/ FACTORS
# reading the file
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)





# STEP 2 - EXPLORING AND PREPARING THE DATA
# removing the id field - to make the data more generalized and thus removing overfitting
wbcd <- wbcd[-1]

# checking the overall distribution of prediction/ target charcterstics/feature
table(wbcd$diagnosis)
gmodels::CrossTable(wbcd$diagnosis)

# converting the target feature into factor with adding labels for more information
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

# to just check the proportion 
round(prop.table(table(wbcd$diagnosis))* 100 , digits = 1)

# checking the summary of wbcd
summary(wbcd)

# normalizing the data to standardize range of values of various characterstics
# creating normalized method 
normaize <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normaize))
summary(wbcd_n)

# creating training and testing dataset from exisitng sample
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:length(wbcd$diagnosis),]

# creating lables for test and training data sets
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:length(wbcd$diagnosis),1]




# STEP 3 - TRAINING A MODEL ON THE DATA
library(class)
# using the knn method of class package with K value equalent to square root of 
# total train observation & odd number to eliminate tie vote issue for 2 factor classification
wbcd_test_predict <- class::knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)




# STEP 4 - EVALUATING MODEL PERFORMANCE
gmodels::CrossTable(x = wbcd_test_labels, y = wbcd_test_predict, prop.chisq = FALSE)
# able to categorize data 98% times - 2% False Negative



# STEP 5 - IMPROVING MODEL PERFORMANCE
#Method 1
# by changing k values
i <- 27
while (i > 0) {
  wbcd_test_predict <- class::knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = i)
  print(i)
  gmodels::CrossTable(x = wbcd_test_labels, y = wbcd_test_predict, prop.chisq = FALSE)
  i <- i - 1
}
# only in case of k = 1, the False Negative is 1% but adding False Postive by 3%.



# Method 2 - used scale inbulit R method to scale the method at place of normalization using z score
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# creating training and testing dataset from exisitng sample
wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:length(wbcd$diagnosis),]

# trainging the model again
wbcd_test_predict <- class::knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

# evaluating
gmodels::CrossTable(x = wbcd_test_labels, y = wbcd_test_predict, prop.chisq = FALSE)
# using scale we were only able to categorise 95% only - 5% False Negative

# Moreover, we should not formulate our approach too closely to our test data.