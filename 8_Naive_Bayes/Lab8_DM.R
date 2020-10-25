# Naive Bayes

# exploring data
titanicData <- read.csv("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/8_Naive_Bayes/titanic.csv", header = T,
                    na.strings = c(""), stringsAsFactors = TRUE)
str(titanicData)

# converting few data to factors
titanicData$Survived <- factor(titanicData$Survived,
                               levels = c(0, 1), labels = c("No", "Yes"))

titanicData$Pclass <- as.factor(titanicData$Pclass)

# removing column 1 and 11
titanicData <- titanicData[, -c(1, 11)]

# setting value of Emaarked at index 62, 830
titanicData$Embarked[c(62, 830)] <- "C"

# checking non- factor variables
table(titanicData$SibSp)
table(titanicData$Parch)

# converitng above 2 to factors /  as it has less factors
titanicData$SibSp <- as.factor(titanicData$SibSp)
titanicData$Parch <- as.factor(titanicData$Parch)

# checking other non-factor variables
par(mfrow=c(1, 2))
hist(titanicData$Fare, breaks = 30)
hist(titanicData$Age)

# converting fare to multiple bins
titanicData$FareBinned <- cut(x = titanicData$Fare, breaks = c(0, 10, 50, max(titanicData$Fare)), labels = c("Low", "Middle", "High"))
table(titanicData$FareBinned, titanicData$Pclass)

# checking combined summary
aggregate(Fare ~ Pclass, data = titanicData, FUN=summary)

# for filling the NA's in age
library(mice)
# using random forest
mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               c("PassengerId", "Name", "Ticket", "Cabin", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age

# coverting age to fators
titanicData$AgeBinned <- cut(titanicData$Age,
                             breaks = c(0, 10, 20, 30, 40, 50, 60, 70, max(titanicData$Age)),
                             labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))
table(titanicData$AgeBinned)

