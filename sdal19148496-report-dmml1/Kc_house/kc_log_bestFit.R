setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Kc_house/")
source("Kc_Import_Explore_Clean.R")
source("kc_Transform_log.R")

# checking correlation
pairs.panels(kc_house_log)


# removing very less correlated variable by creating a new df
house <- kc_house_log
# remove sqft_lot, floors, waterfront, view, condition, sqft_basement, yr_built, 
#yr_renovated i.e columns (5,6,7,8,9,13,14)
house <- house[,-c(5,6,7,8,9,12,13,14)]


# Model the linear model
# simple model
house.fit1 <- lm(price ~ bedrooms*bathrooms*sqft_living*grade*sqft_above, data = house)
summary(house.fit1)
par(mfrow = c(2,2))
plot(house.fit1)

# fit 2
house.fit2 <- update(house.fit1, ~ . + I(bedrooms^2) + I(bathrooms^2) + I(sqft_living^2) + I(grade^2) + I(sqft_above^2))
summary(house.fit2)

# fit 3
house.fit3 <- update(house.fit2, ~ . + I(bedrooms^3) + I(bathrooms^3) + I(sqft_living^3) + I(grade^3) + I(sqft_above^3))
summary(house.fit3)

# fit 4
house.fit4 <- update(house.fit3, ~ . + I(bedrooms^0.5) + I(bathrooms^0.5) + I(sqft_living^0.5) + I(grade^0.5) + I(sqft_above^0.5))
summary(house.fit4)

# fit 4 : step function
house.fit5 <- step(house.fit4)
summary(house.fit5)

# fit 6 :  removing non - significant coefficients manually
house.fit6 <- update(house.fit5, ~ . - bedrooms:sqft_above - bedrooms:sqft_living - bedrooms:bathrooms)
summary(house.fit6)

# fit 7 : step function
house.fit7 <- update(house.fit6, ~ . - bathrooms)
summary(house.fit7)

# from model 7 formula : 
#lm(formula = price ~ bedrooms + sqft_living + grade + sqft_above + 
# I(bedrooms^2) + I(bathrooms^2) + I(sqft_living^2) + I(sqft_above^2) + 
#    I(bedrooms^3) + I(bathrooms^3) + I(sqft_living^3) + I(grade^3) + 
#    I(sqft_above^3) + I(bedrooms^0.5) + I(bathrooms^0.5) + I(grade^0.5) + 
#    bathrooms:sqft_living + bedrooms:grade + bathrooms:grade + 
#    sqft_living:grade + bathrooms:sqft_above + sqft_living:sqft_above + 
#    grade:sqft_above + bedrooms:bathrooms:sqft_living + bedrooms:bathrooms:sqft_above + 
#    bathrooms:sqft_living:sqft_above + bathrooms:grade:sqft_above + 
#    sqft_living:grade:sqft_above, data = house) 


# check the best fit
bstFits1 <- regsubsets(price ~ bedrooms + sqft_living + grade + sqft_above + 
                         I(bedrooms^2) + I(bathrooms^2) + I(sqft_living^2) + I(sqft_above^2) + 
                         I(bedrooms^3) + I(bathrooms^3) + I(sqft_living^3) + I(grade^3) + 
                         I(sqft_above^3) + I(bedrooms^0.5) + I(bathrooms^0.5) + I(grade^0.5) + 
                         bathrooms:sqft_living + bedrooms:grade + bathrooms:grade + 
                         sqft_living:grade + bathrooms:sqft_above + sqft_living:sqft_above + 
                         grade:sqft_above + bedrooms:bathrooms:sqft_living + bedrooms:bathrooms:sqft_above + 
                         bathrooms:sqft_living:sqft_above + bathrooms:grade:sqft_above + 
                         sqft_living:grade:sqft_above, data = house, nbest = 1, nvmax = 4)
par(mfrow = c(1,1))
subsets(bstFits1, statistic = "adjr2")
plot(bstFits1, scale = "adjr2")


# Final model - 
house.final <- lm (price ~ sqft_living + grade + sqft_above, data = house)
summary(house.final)
par(mfrow = c(2,2))
plot(house.final)

# removing the influencial points
house <- house[- c(12778),]

# same model again
house.final <- lm (price ~ sqft_living + grade + sqft_above, data = house)
summary(house.final)
par(mfrow = c(2,2))
plot(house.final)



library(boot)
# checking the accuracy using k-Fold method
house.glm <- glm(price ~ sqft_living + grade + sqft_above, data = house)
summary(house.glm) # AIC = 15670
house.glm.err<- cv.glm(data = house,glmfit = house.glm, K = 10)
round(house.glm.err$delta[1],4)# 0.1209