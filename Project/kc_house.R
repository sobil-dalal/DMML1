setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/DM_DATA_SETS")
remove(list = ls())

library(psych)
library(leaps)
library(car)

# reading the raw csv file
kc_house <- read.csv("kc_house_data.csv", stringsAsFactors = FALSE)

# exploratory analysis 
head(kc_house)
str(kc_house)
summary(kc_house)



# cleaning & removing the unrelated and insignificant data
# removing id
kc_house <- kc_house[,-c(1)]

# coverting the date field type from chr to date
kc_house$date <- as.Date(kc_house$date, format = "%Y%m%d")
summary(kc_house$date)
# Range => Min date - "2014-05-02" to Max = "2015-05-27"
# Almost for the same duration of time, so the sample is unbiased
# remove date
kc_house <- kc_house[,-c(1)]

# removing the lat, long, zipcode
kc_house <- kc_house[,-c(15,16,17)]

# sqft_living, sqft_lot are related and can calculate sqft_living15, sqft_lot15 resp.
kc_house <- kc_house[,-c(15,16)]

# converting the waterfront to factors (0,1)
table(kc_house$waterfront)
kc_house$waterfront <- as.factor(kc_house$waterfront)




# cheking NAs
apply(X = kc_house,MARGIN =  2, FUN = function(col) any(is.na(col))) # no NAs

# checking distribution & outliers of dependent value
par(mfrow = c(1,2))
hist(kc_house$price,
     col="orange", 
     border="black",
     prob = TRUE,
     xlab = "House prices",
     main = "Histogram")
lines(density(kc_house$price),
      lwd = 2,
      col = "chocolate3") # not distributed normally - right skewed
plot(kc_house$price, main = "Scatter plot", ylab = "House prices")

# covert to log
hist(log(kc_house$price),
     col="orange", 
     border="black",
     prob = TRUE,
     xlab = "House prices",
     main = "Histogram")
lines(density(log(kc_house$price)),
      lwd = 2,
      col = "chocolate3") # not distributed normally
plot(log(kc_house$price), main = "Scatter plot", ylab = "House prices")



# creating a new data frame with correlated variable only
kc_house_log <- kc_house 
kc_house_log$price <- log(kc_house_log$price)
summary(kc_house_log)

# checking correlation
pairs.panels(kc_house_log)


# removing very less correlated variable by creating a new df
house <- kc_house_log
# remove sqft_lot, floors, waterfront, view, condition, sqft_basement, yr_built, 
#yr_renovated i.e columns (5,6,7,8,9,13,14)
house <- house[,-c(5,6,7,8,9,12,13,14)]
   

# exploring data
summary(house)

# exploring each colmuns
table(house$bedrooms) # 33 bedroom seems to be a influential point
house <- house[house$bedrooms != 33,]

table(house$bathrooms)

table(house$view)

row.names(house) <- 1:21612

 
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
summary(house.glm)
house.glm.err<- cv.glm(data = house,glmfit = house.glm, K = 10)
house.glm.err$delta




# new model log
new.fit.lm <- lm(price ~ bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+sqft_above+yr_built, data = kc_house_log)
summary(new.fit.lm)
par(mfrow = c(2,2))
plot(new.fit.lm)
new.fit.glm <- glm(price ~ bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+sqft_above+yr_built, data = kc_house_log)
summary(new.fit.glm)
house.glm.err<- cv.glm(data = kc_house_log,glmfit = new.fit.glm, K = 10)
house.glm.err$delta

