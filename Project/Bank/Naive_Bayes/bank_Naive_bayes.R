setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Bank/")

library(ggplot2)
library(ggthemes)
library(caret)
library(e1071)

# Step 1 : Collect data - 
source("bank_import_primaryExplore.R")

# Step 2 : Explore, clean and prepare the data
# primary exploration done in step 1
# creating a new df to convert the numeric columns (continous) to factors by binning
bank_n <- bank
summary(bank_n)

# checking histograms and creating factors
theme_set(theme_gdocs())

# age
pl.age <- ggplot(bank_n, aes(x=age)) + geom_histogram(bins = 10, color = 'blue', aes(fill=..count..), alpha = 0.4) + xlab('Age') + ylab('Count') + ggtitle('Age distribution Plot')
pl.age
bank_n$age <- cut(bank_n$age,
                  breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, max(bank_n$age)),
                  labels = c("1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80+"))
table(bank_n$age)

# duration
pl.duration <- ggplot(bank_n, aes(x=duration)) + geom_histogram(bins = 15, color = 'blue', aes(fill=..count..), alpha = 0.4) + xlab('Duration') + ylab('Count') + ggtitle('Duration distribution Plot') +stat_bin(breaks=c(-0.004, seq(0.001,1.0, by=0.005)))
pl.duration
summary(bank_n$duration)
bank_n$duration <- cut(bank_n$duration,
                  breaks = c(0, 10, 30, 70, 110, 150, 190, 250, 350, 600, 900, 1200, max(bank_n$duration)),
                  labels = c("1-10","30-70", "30-70", "70-110", "110-150", "150-190", "190-250", "250-350", "350-600", "600-900", "900-1200", "1200+"))
table(bank_n$age)

# campaign
pl.campaign<- ggplot(bank_n, aes(x=campaign)) + geom_histogram(bins = 15, color = 'blue', aes(fill=..count..), alpha = 0.4) + xlab('campaign') + ylab('Count') + ggtitle('campaign distribution Plot')
pl.campaign
summary(bank_n$campaign)
bank_n$campaign <- cut(bank_n$campaign,
                       breaks = c(1, 2, 3, 5, 10, 15, 20, max(bank_n$campaign)),
                       labels = c("1-2","2-3", "3-5", "5-10", "10-15", "15-20", "20+"))
table(bank_n$campaign)

# pdays
summary(bank_n$pdays)
table(bank_n$pdays)
bank_n$pdays <- cut(bank_n$pdays,
                       breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 28, max(bank_n$pdays)),
                       labels = c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10-15", "15-20", "20-28", "999"))
summary(bank_n$pdays)

# previous
summary(bank_n$previous)
table(bank_n$previous)
bank_n$previous <- as.factor(bank_n$previous)
table(bank_n$previous)

# emp.var.rate
summary(bank_n$emp.var.rate)
table(bank_n$emp.var.rate)
bank_n$emp.var.rate <- as.factor(bank_n$emp.var.rate)
summary(bank_n$emp.var.rate)

#cons.price.idx
summary(bank_n$cons.price.idx)
table(bank_n$cons.price.idx)
hist(bank_n$cons.price.idx, col = "lightblue")
bank_n$cons.price.idx <- cut(bank_n$cons.price.idx,
                    breaks = c(92,93, 93.5, 94, max(bank_n$cons.price.idx)),
                    labels = c("92-93", "93-93.5", "93.5-94", "94+"))
summary(bank_n$cons.price.idx)

# cons.conf.idx
summary(bank_n$cons.conf.idx)
table(bank_n$cons.conf.idx)
hist(bank_n$cons.conf.idx, col = "lightblue")
bank_n$cons.conf.idx <- cut(bank_n$cons.conf.idx,
                             breaks = c(-51, -45, -40, -35, max(bank_n$cons.conf.idx)),
                             labels = c("(-50)-(-45)", "(-45)-(-40)", "(-40)-(-35)", "(-35)+"))
summary(bank_n$cons.conf.idx)

# euribor3m
summary(bank_n$euribor3m)
pl.euribor3m <- ggplot(bank_n, aes(x=euribor3m)) + geom_histogram(bins = 10, color = 'blue', aes(fill=..count..), alpha = 0.4) + xlab('euribor3m') + ylab('Count') + ggtitle('euribor3m distribution Plot')
pl.euribor3m
bank_n$euribor3m <- cut(bank_n$euribor3m,
                             breaks = c(0,1, 2, 4, max(bank_n$euribor3m)),
                             labels = c("0-1", "1-2", "2-4", "4+"))
summary(bank_n$euribor3m)

# nr.employed
summary(bank_n$nr.employed)
table(bank_n$nr.employed)
bank_n$nr.employed <- as.factor(bank_n$nr.employed)
summary(bank_n$nr.employed)



# summary of df
summary(bank_n)

# removing NAs
bank_n <- na.omit(bank_n)
summary(bank_n)

# creating training and testing dataset from exisitng sample
indx <- createDataPartition(bank_n$y, p = 0.8, list = FALSE)

# creating lables for test and training data sets
bank_train_labels <- bank_n[indx,21]
bank_test_labels <- bank_n[- indx,21]

# removing dependent variable
bank_n <- bank_n[,-21]
summary(bank_n)

bank_train <- bank_n[indx,]
bank_test <- bank_n[- indx,]

# Step 3 : Train the model
bank_classifier <- naiveBayes(bank_train, bank_train_labels)



# Step 4 : Evaluate the model
bank_test_pred <- predict(bank_classifier, bank_test)
caret::confusionMatrix(bank_test_pred, bank_test_labels)


# Step 5 : Improving the model
bank_classifier2 <- naiveBayes(bank_train, bank_train_labels, laplace = 1)
bank_test_pred2 <- predict(bank_classifier2, bank_test)
caret::confusionMatrix(bank_test_pred2, bank_test_labels)
"
Confusion Matrix and Statistics

          Reference
Prediction  Yes   No
       Yes  277  367
       No   190 3873
                                          
               Accuracy : 0.8817          
                 95% CI : (0.8721, 0.8908)
    No Information Rate : 0.9008          
    P-Value [Acc > NIR] : 1               
                                          
                  Kappa : 0.4335          
                                          
 Mcnemar's Test P-Value : 8.827e-14       
                                          
            Sensitivity : 0.59315         
            Specificity : 0.91344         
         Pos Pred Value : 0.43012         
         Neg Pred Value : 0.95324         
             Prevalence : 0.09921         
         Detection Rate : 0.05885         
   Detection Prevalence : 0.13682         
      Balanced Accuracy : 0.75330         
                                          
       'Positive' Class : Yes             
                                          
"
