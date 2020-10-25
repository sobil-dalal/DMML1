x <- rnorm(50)
y <- rnorm(x)

plot(x,y)

boxplot(x)
hist(x)


x <- c(10.4, 5.6, 3.1, 6.4, 21.7)

print(x)

y <- range(x)
print(y)


# Tasks
# 1 - Identify the values of x between 3 and 7
x <- 1:20
y <- (x > 3) & (x <7)
print(x[y])

# or
y <- which(x > 3 & x < 7)
print(y)

#2. Identify which values of x are greater than the mean of x
x.mean <- mean(x)
x.grtThnMean <- which(x > x.mean)
print(x.grtThnMean)

#3. Identify which values of x are more than 1 standard deviation from the mean of x. Hint: ?sd
x.sdGrt <- which(x < x.mean - sd(x) | x > x.mean + sd(x))
print(x.sdGrt)






