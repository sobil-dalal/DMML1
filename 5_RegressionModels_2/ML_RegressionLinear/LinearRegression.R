remove(list = ls())
setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/5_RegressionModels_2/ML_RegressionLinear/")

# Step 1 - Collecting Data - 
insurance <- read.csv("insurance.csv")





# Step 2 - Exploring and preparing the data
str(insurance)
summary(insurance)
# for linear regression the variables should be numerixc whereas we have 3 factor features in the data sets
# mean of expenses is greater than median thus expenses are right-skewed
# cheking above by plotting the histogram 
hist(x = insurance$expenses)
# histogram using ggplot2
library(ggplot2)
expenses.ggplt <- ggplot(data = insurance, aes(expenses))
expenses.ggplt <- expenses.ggplt + ggplot2::geom_histogram(bins = 12, aes(fill = ..count.. , color = ..count..) )  + scale_color_gradient(low = 'blue', high = 'red')
expenses.ggplt

# Exploring relationships amoung features using correlation matrix
cor(insurance[c("age", "bmi", "children", "expenses")])

# visualizing the data and correlation
pairs(insurance)
# visualizing the data and correlation - particular features
pairs(insurance[c("age", "bmi", "children", "expenses")])

# enhanced scatter plot matrix using psych package
library(psych)
psych::pairs.panels(insurance[c("age", "bmi", "children", "expenses")])




# Step 3 - Training a model on the data
ins_model <- lm(expenses ~ . , data = insurance)
ins_model




# Step 4 - Evaluating the model performance
summary(ins_model)



# Step 5 - Improving the model performance
# age above 60 has more expense - can be esquare
insurance$age2 <- insurance$age^2

# bmi above 30 relation check : creating binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# checking interaction between bmi and smoking with above 2 in new lm
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)













