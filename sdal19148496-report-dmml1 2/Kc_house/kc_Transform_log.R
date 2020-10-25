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