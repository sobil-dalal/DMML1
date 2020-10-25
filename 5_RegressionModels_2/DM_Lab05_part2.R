remove(list = ls())
library(ISLR)
data("Hitters")
Hitters <- na.omit(Hitters)
x <- model.matrix(Salary ~.,Hitters)[,-1]
y <- Hitters$Salary
set.seed(1)
train <- sample(x = 1:nrow(x), size = nrow(x)/2)
test <- (-train)
y.test <- y[test]


# PCR
library(pls)
set.seed(2)
pcr.fit <- pls::pcr(Salary ~ . , data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type = "MSEP")


# evaluating the training data
pcr.fit <- pcr(Salary ~ . , data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# Now we find that the lowest cross-validation error occurs when M = 7 component are used. We compute the test MSE as follows.
pcr.pred <- predict(object = pcr.fit, newdata = x[test,], ncomp = 7)
# calculating mean
mean((pcr.pred - y.test)^2)

# fitting complete data on M=7
pcr.fit <- pls::pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)





# PLS modeling
set.seed(1)
pls.fit <- pls::plsr(Salary ~ . , data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

pls.predict <- predict(pls.fit , newdata = x[test,], ncomp = 2)
summary(pls.predict)


# checking MSEP
mean((pls.predict - y.test)^2)


# for whole data set
pls.fit <- pls::plsr(Salary ~ . , data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)
