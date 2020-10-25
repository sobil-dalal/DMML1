remove(list = ls())
setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/5_RegressionModels_2/ML_RegressionLinear/")

launch <- read.csv("challenger.csv")

# calculating the coefficients
# b = covariance(x,y) / variance (X)
b = cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
# a = mean(y) - b*mean(x)
a = mean(launch$distress_ct) - b*mean(launch$temperature)
paste("a is equivalent to beta-o = ", a)
paste("b is equivalent to beta-1 = ", b)

# correlation coefficients
#p(x,y) = Corr(x,y) = Cov(x,y)/(sd(x) * sd(y)
r <- cov(launch$temperature, launch$distress_ct) / (sd(launch$temperature) * sd(launch$distress_ct))
paste("correlation coefficient = p(x,y) = r  = ", r)

# can also be calculated using inbuilt R function
paste("correlation coefficient = p(x,y) = r  = ", cor(x = launch$temperature, y = launch$distress_ct))
paste("Is r calculated equal to methods evaluated correlation = ", r == cor(launch$temperature, launch$distress_ct))



# creating regression function / method
reg <- function(x,y) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1 , x)
  # beta = (X-transpose * X)^-1 * (X-transpose * Y)
  b <- solve(t(x) %*% x) %*% (t(x) %*% y)
  colnames(b) <- "estimates"
  return(b)
}

# calculating the coefficients - for single independent variable
reg(x = launch[2], y = launch$distress_ct)

# calculating rhe coefficients using own defined reg function  - for multiple variables
reg(x = launch[2:4], y = launch$distress_ct)