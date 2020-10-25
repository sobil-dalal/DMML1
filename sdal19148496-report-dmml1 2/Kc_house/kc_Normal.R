setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Kc_house/")
source("Kc_Import_Explore_Clean.R")

library(boot)
library(car)
library(leaps)

# checking correlation
pairs.panels(kc_house)

# Model the linear model
# simple model 1
house.fit1 <- glm(price ~ ., data = kc_house)
summary(house.fit1)

# Model 2 - step
house.fit2 <- step(house.fit1)
summary(house.fit2)

# Model 3 - interaction
house.fit3 <- update(house.fit2, ~ . + bedrooms:bathrooms + bedrooms:grade + bedrooms:sqft_above + bedrooms:sqft_living + bathrooms:grade + bathrooms:sqft_above + sqft_above:sqft_living + grade:sqft_living + bathrooms:sqft_living + floors:sqft_living + view:sqft_living + floors:condition + floors:grade + floors:sqft_above + waterfront:view + view:grade)
summary(house.fit3)

# Model 4 - step
house.fit4 <- step(house.fit3)
summary(house.fit4)

# Model 5
house.fit5 <- update(house.fit2, ~ . - floors)
summary(house.fit5)




# check the best fit by ignoring uncorrelated variables
bstFits1 <- regsubsets(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated, data = kc_house, nbest = 1, nvmax = 6)
par(mfrow = c(1,1))
subsets(bstFits1, statistic = "adjr2", max.size = 6)
plot(bstFits1, scale = "adjr2")



# Doing cross-validation for 4,5,6 predictors using K-Fold
# 3 predictors
house.k10.4 <- glm(price ~ sqft_living + waterfront + yr_renovated + grade, data = kc_house)
summary(house.k10.4) # 596532
house.k10.4.err <- cv.glm(data = kc_house,glmfit = house.k10.4, K = 10)
round(house.k10.4.err$delta[1], 4) # 56952103727





