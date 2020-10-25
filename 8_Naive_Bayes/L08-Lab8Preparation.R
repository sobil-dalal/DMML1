titanicData <- read.csv("titanic.csv", header = T,
  na.strings = c(""), stringsAsFactors = T)

titanicData$Survived <- factor(titanicData$Survived,
  levels = c(0, 1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)
titanicData <- titanicData[, -c(1, 11)]

titanicData$Embarked[c(62, 830)] <- "C"

table(titanicData$SibSp)
table(titanicData$Parch)

titanicData$SibSp <- as.factor(titanicData$SibSp)
titanicData$Parch <- as.factor(titanicData$Parch)

par(frow=c(1, 2))
hist(titanicData$Fare, breaks = 30)
hist(titanicData$Age)

titanicData$FareBinned <- cut(titanicData$Fare,
  breaks = c(0, 10, 50, max(titanicData$Fare)),
  labels = c("Low", "Middle", "High"))
table(titanicData$FareBinned, titanicData$Pclass)

aggregate(Fare ~ Pclass, data = titanicData, FUN=summary)

library(mice)
mice_mod <- mice(titanicData[, !names(titanicData) %in%
  c("PassengerId", "Name", "Ticket", "Cabin", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age

titanicData$AgeBinned <- cut(titanicData$Age,
  breaks = c(0, 10, 20, 30, 40, 50, 60, 70, max(titanicData$Age)),
  labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70+"))
table(titanicData$AgeBinned)