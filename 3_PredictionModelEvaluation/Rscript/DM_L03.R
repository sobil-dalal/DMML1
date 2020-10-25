remove(list = ls())
setwd('/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/3_PredictionModelEvaluation//Rscript/')
titanicData <- read.csv(file = "titanic.csv", header = TRUE, na.strings = "", stringsAsFactors = TRUE)

# Task 2
titanicData$Survived <- as.factor(titanicData$Survived)
titanicData$Pclass <- as.factor(titanicData$Pclass)

# Task 3
sapply(titanicData, function(x) sum(is.na(x)))
# or check visually
library(Amelia)
missmap(obj = titanicData, main = "Missing values vs observed")

# handing missing data of embarked (Total 2 values missing)
embarkedNAs <- titanicData %>% filter(PassengerId == 62 | PassengerId == 830)
print(embarkedNAs[, c(3,10,4)])
passengerId.missing <- titanicData[is.na(titanicData$Embarked), 1]
print(passengerId.missing)
library(dplyr)
embarked <- titanicData %>%
  filter(PassengerId != 62 & PassengerId != 830)

library(ggplot2)
library(ggthemes)
library(scales)
# Use ggplot2 to visualize embarkment, passenger class, & median fare 
ggplot(embarked, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
geom_boxplot() +
  geom_hline(aes(yintercept=80), #<--- the price we know for one passenger
             colour='red', linetype='dashed', lwd=2) + scale_y_continuous(labels=dollar_format()) + theme_few()
titanicData$Embarked[c(62, 830)] <- 'C'
sum(is.na(titanicData$Embarked))


# Handling multiple missing values
ImputedAgeMean <- titanicData$Age


# Task Impute the missing values of ImputedAgeMean to be the mean age (make sure that you don’t keep missing values in the computation of the mean!).
ImputedAgeMean[is.na(ImputedAgeMean)] <- mean(x = ImputedAgeMean, na.rm = TRUE)

# Task Use histograms to inspect the distribution of titanicData$Age compared to ImputedAgeMean. Would you happy with the result? If you are, you probably left the missing values in!!
par(mfrow=c(1,2)) 
hist(titanicData$Age)
hist(ImputedAgeMean)

# Chosing the mean has not retained the same distribution, in fact it has made the peak (values around the mean) more frequent: compare the y-axes!!
# Replace mean with median
ImputedAgeMedian <- titanicData$Age
ImputedAgeMedian[is.na(ImputedAgeMedian)] <- median(ImputedAgeMedian, na.rm = TRUE) 
par(mfrow=c(1,2))
hist(titanicData$Age)
hist(ImputedAgeMedian)

# Looks largely the same, now: let’s check some descriptive statistics of the data:
df <- data.frame(titanicData$Age, ImputedAgeMean, ImputedAgeMedian)
summary(df)

# checking standard deviation
sapply(df, function(x) sd(x, na.rm=TRUE))




# Let’s check, first we need to build the LM and mice models.
idx_na <- is.na(titanicData$Age)
age_train <- titanicData[-idx_na, ]
age_test <- titanicData[idx_na, ]

ageModel <-lm(Age~Pclass + Survived + SibSp, data = age_train)
age_test$Age <- predict(ageModel, newdata = age_test)


ImputedAgeLM <- titanicData
ImputedAgeLM[ImputedAgeLM$PassengerId %in% age_test$PassengerId, "Age"] <- age_test$Age


# using mice package
library(mice)
# Perform mice imputation, excluding some variables that probably won't help: 
mice_mod <- mice(titanicData[, !names(titanicData) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
# Complete the missing values
mice_output <- complete(mice_mod)
df <- data.frame(titanicData$Age, ImputedAgeMean, ImputedAgeMedian, ImputedAgeLM$Age, mice_output$Age)
summary(df)
sapply(df, function(x) sd(x, na.rm=T))

# So, we see that the LM approach has produced a few negative ages – we clearly don’t want that! The mice model, however, is pretty good, the mean, and standard deviation are more or less retained.

par(mar = c("1","1","1","1"))
# Make a barplot of the survived variable, to gauge the survival rate.
barplot(table(titanicData$Survived))

#Make a barplot of the Sex variable, to see the distribution of gender.
barplot(table(titanicData$Sex))

# Try out different combinations of Survived with other explanatory variables
library(ggplot2)
library(ggthemes)
ggplot(titanicData, aes(Pclass, fill = factor(Survived))) + geom_bar(stat='count', position='dodge') + facet_grid(.~Sex) +
  theme_few()

ggplot(titanicData, aes(Pclass, fill = factor(Survived))) + geom_bar(stat='count', position='dodge') + facet_grid(.~Embarked) +
  theme_few()


# creating groups using continous data
#first inspect Fare
hist(titanicData$Fare)

titanicData$binnedFare <- cut(titanicData$Fare, breaks=c(-1, 0, 10, 25, 50, 100, 600), labels = c("Free", "Cheapest", "Cheaper", "Mid Range", "Expensive", "Most Expensive"))
ggplot(titanicData, aes(binnedFare, fill = factor(Survived))) + geom_bar(stat='count', position='dodge') +
  facet_grid(.~Sex) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Create a new factor called Child
titanicData$Child[titanicData$Age < 18] <- "Y"
titanicData$Child[titanicData$Age >= 18] <- "N"
titanicData$Child <- as.factor(titanicData$Child)

# Create the plot below with the new Child column.
ggplot(titanicData, aes(Child, fill = factor(Survived))) + geom_bar(stat='count', position='dodge') + facet_grid(.~Sex) +
  theme_few()

# Make the plot below, to help us understand if Fsize has any rela- tionship with survival
ggplot(titanicData, aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()
