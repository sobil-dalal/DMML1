setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Kc_house/")
source("Kc_Import_Explore_Clean.R")
source("kc_Transform_log.R")

library(boot)
library(car)
library(leaps)

# checking correlation
pairs.panels(kc_house_log)

# model log 1
house.glm1 <- glm(price ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+yr_built+yr_renovated, data = kc_house_log)
summary(house.glm1)

# model 2
house.glm2 <- update(house.glm1, ~ . - sqft_basement)
summary(house.glm2)

# model 3
house.glm3 <- update(house.glm2, ~ . - sqft_lot)
summary(house.glm3)

# model 4
house.glm4 <- update(house.glm3, ~ . - yr_renovated)
summary(house.glm4)

# adding interaction by checking the graph
# model 5 - sqft_living interaction
house.glm5 <- update(house.glm4, ~ . + sqft_above:sqft_living + grade:sqft_living + bathrooms:sqft_living + floors:sqft_living + view:sqft_living)
summary(house.glm5)

# model 6
house.glm6 <- update(house.glm5, ~ . - bathrooms:sqft_living)
summary(house.glm6)

# model 7 - bedrooms interaction
house.glm7 <- update(house.glm6, ~ . + bedrooms:bathrooms + bedrooms:grade + bedrooms:sqft_above)
summary(house.glm7)

# model 8
house.glm8 <- update(house.glm7, ~ . - bedrooms:sqft_above - bedrooms:bathrooms - sqft_living:sqft_above)
summary(house.glm8)

# model 9 - bathrooms interaction
house.glm9 <- update(house.glm8, ~ . + bathrooms:floors + bathrooms:grade + bathrooms:sqft_above)
summary(house.glm9)

# model 10
house.glm10 <- update(house.glm9, ~ . - bathrooms:grade - bathrooms:floors)
summary(house.glm10)

# model 11 - floors interaction
house.glm11 <- update(house.glm10, ~ . + floors:condition + floors:grade + floors:sqft_above)
summary(house.glm11)

# model 12
house.glm12 <- update(house.glm11, ~ . - sqft_living:floors - floors:condition -floors:grade)
summary(house.glm12)

# model 13
house.glm13 <- update(house.glm12, ~ . - sqft_above)
summary(house.glm13)

# model 14 - floors interaction
house.glm14 <- update(house.glm13, ~ . + waterfront:view + view:grade + grade:sqft_above)
summary(house.glm14)

# model 15
house.glm15 <- update(house.glm14, ~ . - grade:sqft_above - view:grade - waterfront)
summary(house.glm15)



# as per concept of parsimony
# checking best fit from above model
house.glm15.bestFit1 <- regsubsets(price ~ bedrooms + bathrooms + sqft_living + floors + 
                                  view + condition + grade + yr_built + sqft_living:grade + 
                                  sqft_living:view + bedrooms:grade + bathrooms:sqft_above + 
                                  floors:sqft_above + waterfront:view, data = kc_house_log, nbest = 2, nvmax = 10)
par(mfrow = c(1,1))
subsets(house.glm15.bestFit1, statistic = "adjr2")
plot(house.glm15.bestFit1, scale = "adjr2")

# from above we can see 4,5,6 and later produce almost similar result 
house.glm15.bestFit2 <- regsubsets(price ~ bedrooms + bathrooms + sqft_living + floors + 
                                     view + condition + grade + yr_built + sqft_living:grade + 
                                     sqft_living:view + bedrooms:grade + bathrooms:sqft_above + 
                                     floors:sqft_above + waterfront:view, data = kc_house_log, nbest = 1, nvmax = 6)
par(mfrow = c(1,1))
subsets(house.glm15.bestFit2, statistic = "adjr2", max.size = 6, min.size = 1)
plot(house.glm15.bestFit2, scale = "adjr2")


# Doing cross-validation for 3,4,5,6 predictors using K-Fold
# 3 predictors
house.k10.3i <- glm(price ~ sqft_living + grade + yr_built, data = kc_house_log)
summary(house.k10.3i) # 12786
house.k10.3i.err <- cv.glm(data = kc_house_log,glmfit = house.k10.3i, K = 10)
round(house.k10.3i.err$delta[1], 4) # 0.1058

house.k10.3ii <- glm(price ~ sqft_living:grade + grade + yr_built, data = kc_house_log)
summary(house.k10.3ii) # 13539
house.k10.3ii.err <- cv.glm(data = kc_house_log,glmfit = house.k10.3ii, K = 10)
round(house.k10.3ii.err$delta[1], 4) # 0.1096


# 4 predictors
house.k10.4i <- glm(price ~ sqft_living + grade + yr_built + view, data = kc_house_log)
summary(house.k10.4i) # 12272
house.k10.4i.err <- cv.glm(data = kc_house_log,glmfit = house.k10.4i, K = 10)
round(house.k10.4i.err$delta[1], 4) # 0.1033

house.k10.4ii <- glm(price ~ sqft_living + grade + yr_built + bathrooms, data = kc_house_log)
summary(house.k10.4ii) # 12351
house.k10.4ii.err <- cv.glm(data = kc_house_log,glmfit = house.k10.4ii, K = 10)
round(house.k10.4ii.err$delta[1], 4) # 0.1037


# 5 predictors
house.k10.5i <- glm(price ~ sqft_living + grade + yr_built + view + bathrooms, data = kc_house_log)
summary(house.k10.5i) # 11844
house.k10.5i.err <- cv.glm(data = kc_house_log,glmfit = house.k10.5i, K = 10)
round(house.k10.5i.err$delta[1], 4) # 0.1013


house.k10.5ii <- glm(price ~ sqft_living + grade + yr_built + view + bathrooms, data = kc_house_log)
summary(house.k10.5ii) # 11844
house.k10.5ii.err <- cv.glm(data = kc_house_log,glmfit = house.k10.5ii, K = 10)
round(house.k10.5ii.err$delta[1], 4) # 0.1013


# 6 predictors
house.k10.6i <- glm(price ~ sqft_living + grade + yr_built + view + bathrooms + floors, data = kc_house_log)
summary(house.k10.6i) # 11587
house.k10.6i.err <- cv.glm(data = kc_house_log,glmfit = house.k10.6i, K = 10)
round(house.k10.6i.err$delta[1], 4) # 0.1001

house.k10.6ii <- glm(price ~ sqft_living + grade + yr_built + view + sqft_living:grade + floors, data = kc_house_log)
summary(house.k10.6ii) # 11589
house.k10.6ii.err <- cv.glm(data = kc_house_log,glmfit = house.k10.6ii, K = 10)
round(house.k10.6ii.err$delta[1], 4) # 0.1001
