library(MASS)
library(ISLR)
data("Boston")
options(scipen=999)

# linear modeling between medv : median value of owner-occupied homes in \$1000s to lstat : lower status of the population (percent).
lm.fit = lm(medv~lstat, data = Boston )
lm.fit
summary(lm.fit)
# coefficients 
coef(lm.fit)
lm.fit$coefficients
# confidence intervals
confint(lm.fit)
# names of lm.fit
names(lm.fit)

# prediction to evaluate C.I. for medv for given lstat
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "confidence")

# prediction to evaluate predicition interval for medv for given lstat
predict(lm.fit, data.frame(lstat = c(5,10,15)), interval = "predict")

# prediction interval are wider

# to evaluate further, we will plot the medv and lstat & create least sqaure regression line with abline
plot(x = Boston$lstat, y = Boston$medv, col = "Grey", pch = 1:20)
abline(lm.fit,lwd = 3, col ="Red")

# using par function to load multiple plots at once in a matrix format
par(mfrow = c(2,2))
plot(lm.fit)

# indivializally plotting the function
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues (lm.fit))
which.max(hatvalues (lm.fit))

# multi-linear function
lm.fit <- lm(data = Boston, formula = medv ~ lstat + age)
summary(lm.fit)

# regression using all variable
lm.fit <- lm(medv ~ . , data = Boston)
summary(lm.fit)

# removing age for lm
lm.fit <-update(lm.fit, ~.-age)
summary(lm.fit)

# interaction 
summary(lm(formula = medv~lstat*age, data = Boston))

lm.fit <- lm(medv ~ lstat, data = Boston)
# non-linear Transformation
lm.fit2 <- lm(medv ~ lstat +I(lstat^2), data = Boston)
summary(lm.fit2)

# to check / quantify the extent to which the quadratic fit is superior to linear fit, we will use anova method
anova(lm.fit, lm.fit2)

plot(lm.fit2)

# check polynomial 
lm.fit5 <- lm(medv~poly(lstat,5), data = Boston)
summary(lm.fit5)

# check log transformation
lm.fitlog <- lm(medv~log(lstat), data = Boston)
summary(lm.fitlog)





# Qualitative Predictors

data("Carseats")
names(Carseats)

lm.fit <- lm(Sales ~ . +Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)
Carseats


