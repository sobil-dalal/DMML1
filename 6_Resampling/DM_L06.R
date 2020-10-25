library(ISLR)
attach(Auto)

# 5.3.1 The validation set approach
# seed = 1
set.seed(1)
train=sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit,Auto))[-train]^2)
# using poly for square
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
# using poly for cube
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# chossing different data set by changing the seed
# seed =2
set.seed(2)
train=sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit,Auto))[-train]^2)
# using poly for square
lm.fit2 <- lm(mpg ~ poly(horsepower,2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
# using poly for cube
lm.fit3 <- lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)



# 5.3.2 Leave-One-Out Cross-Validation
glm.fit <- glm(mpg~horsepower, data = Auto)
coef(glm.fit)
# we didn't provide the family, thus it work equivalent to linear model
# lets try lm 
lm.fit <- lm(mpg ~ horsepower, data = Auto)
coef(lm.fit)


library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

cv.error = rep(0,5)
for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error



# 5.3.3 K-Fold Cross-Validation
set.seed(17)
cv.error.10 <- rep(0,10)
for (i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto,glm.fit, K = 10)$delta[1]
}
cv.error.10
