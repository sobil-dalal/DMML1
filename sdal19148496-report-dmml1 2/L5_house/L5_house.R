setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/L5_house/")
remove(list = ls())

options(scipen=1)

library(data.table)
library(boot)
library(fpp2)
library(car)
library(leaps)

# reading the raw csv file
house <- fread("HousePrices_HalfMil.csv")
str(house)
summary(house)

# check normal distribution of Prices
par(mfrow = c(1,2))
hist(house$Prices,
     col = "orange",
     border = "black",
     prob = TRUE,
     xlab = "House prices",
     main = "Histogram")
lines(density(house$Prices),
      lwd = 2,
      col = "chocolate3")
house.boxplot <- boxplot(house$Prices, main = "Box plot", ylab = "House prices")

# removing outliers to make more genarlised  model
house.boxplot$out
house <- subset(house, ! house$Prices %in% house.boxplot$out)
house.boxplot <- boxplot(house$Prices, main = "Box plot", ylab = "House prices")
house.boxplot$out
hist(house$Prices,
     col = "orange",
     border = "black",
     prob = TRUE,
     xlab = "House prices",
     main = "Histogram")
lines(density(house$Prices),
      lwd = 2,
      col = "chocolate3")

# re-numbering the rows names
row.names(house) <- 1:499984

# cheking correlation
cor(house)

# making a new df with only correlated columns
chouse <- house
chouse <- chouse[,-c(2,3,6,10,11,14,15)]

chouse.colnames <-colnames(chouse)
chouse.colnames[3] <- "White_Marbel"
chouse.colnames[4] <- "Indian_Marbel"
colnames(chouse) <- chouse.colnames

# checking correlation
cor(chouse)
pairs(chouse)

# linear model
house.fit1 <- glm(Prices ~ . + White_Marbel:Indian_Marbel , data = chouse)
summary(house.fit1) #8882283

# adding the correlation independent variabels
house.fit2 <- update(house.fit1, ~ . - White_Marbel:Indian_Marbel)
summary(house.fit2) #8882283
accuracy(house.fit2)
house.k10.fit2.err <- cv.glm(data = chouse,glmfit = house.fit2, K = 10)
house.k10.fit2.err$delta# 3039673
#checking r2
house.fit2.lm <- lm(Prices ~ ., data = chouse)
summary(house.fit2.lm) # 0.9793




# checking the best fit
house.fit2.bestFit1 <- regsubsets(Prices ~ Area + Baths + White_Marbel + Indian_Marbel + 
                                     Floors + City + Fiber + `Glass Doors`, data = chouse, nbest = 1, nvmax = 6)
par(mfrow = c(1,1))
subsets(house.fit2.bestFit1, statistic = "adjr2", max.size = 6, min.size = 1)
plot(house.fit2.bestFit1, scale = "adjr2")



# 4 predictors
house.fit4 <- glm(Prices ~ White_Marbel + Floors + City + Fiber, data = chouse)
summary(house.fit4) #9783831
house.k10.fit4.err <- cv.glm(data = chouse,glmfit = house.fit4, K = 10)
house.k10.fit4.err$delta# 18446855
#checking r2
house.fit4.lm <- lm(Prices ~ White_Marbel + Floors + City + Fiber, data = chouse)
summary(house.fit4.lm) # 0.8742

# 5 predictors
house.fit5 <- glm(Prices ~ White_Marbel + Floors + City + Fiber + `Glass Doors`, data = chouse)
summary(house.fit5) #9628934
house.k10.fit5.err <- cv.glm(data = chouse,glmfit = house.fit5, K = 10)
house.k10.fit5.err$delta# 13532564
#checking r2
house.fit5.lm <- lm(Prices ~ White_Marbel + Floors + City + Fiber + `Glass Doors`, data = chouse)
summary(house.fit5.lm) # 0.9077

# 6 predictors
house.fit6 <- glm(Prices ~ White_Marbel + Indian_Marbel + Floors + City + Fiber + `Glass Doors`, data = chouse)
summary(house.fit6) #9445048
house.k10.fit6.err <- cv.glm(data = chouse,glmfit = house.fit6, K = 10)
house.k10.fit6.err$delta# 9368131
#checking r2
house.fit6.lm <- lm(Prices ~ White_Marbel + Indian_Marbel + Floors + City + Fiber + `Glass Doors`, data = chouse)
summary(house.fit6.lm) # 0.9361

par(mfrow = c(2,2))
plot(house.fit2)
