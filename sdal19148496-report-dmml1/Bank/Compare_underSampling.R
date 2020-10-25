remove(list = ls())
setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Bank/")
set.seed(1)
library(caret)
library(RWeka)
library(ROCR)
library(e1071)
library(kernlab)
library(dplyr)
library(ROSE)
library(rpart.plot)
library(party)

# reading the file
bank <- read.csv("bank-additional-full.csv", sep = ";")

# converting the target feature into factor with adding labels for more information
bank$y <- factor(bank$y, levels = c("yes", "no"), labels = c("Yes", "No"))

# creating training and testing dataset from exisitng sample
indx <- createDataPartition(bank$y, p = 0.8, list = FALSE)

bank_train <- bank[indx,]
bank_test <- bank[- indx,]

prop.table(table(bank_train$y))
prop.table(table(bank_test$y))


# SVM best model
bank_svm.rbf100 <- ksvm(y ~ . , data = bank_train, kernel = "rbfdot", C = 100, prob.model = TRUE)
bank_predict_svm.rbf100 <- predict(object = bank_svm.rbf100, newdata = bank_test, type = "prob")
svm_pred = prediction(bank_predict_svm.rbf100[,2],bank_test$y)
svm_pref = performance(svm_pred,"tpr","fpr")

# Decision tree
bank_JRip <- JRip(y ~ . , data = bank_train)
bank_predict_JRip <- predict(object = bank_JRip, newdata = bank_test,  type = "prob")
JRip_pred = prediction(bank_predict_JRip[,2],bank_test$y)
JRip_pref = performance(JRip_pred,"tpr","fpr")

bank_predict_JRip_cm <- predict(object = bank_JRip, newdata = bank_test)
cmJRip <- caret::confusionMatrix(bank_predict_JRip_cm, bank_test$y)

# Naive Bayes
bank_n <- bank
bank_n$age <- cut(bank_n$age,
                  breaks = c(1, 10, 20, 30, 40, 50, 60, 70, 80, max(bank_n$age)),
                  labels = c("1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80+"))
bank_n$duration <- cut(bank_n$duration,
                       breaks = c(0, 10, 30, 70, 110, 150, 190, 250, 350, 600, 900, 1200, max(bank_n$duration)),
                       labels = c("1-10","30-70", "30-70", "70-110", "110-150", "150-190", "190-250", "250-350", "350-600", "600-900", "900-1200", "1200+"))
bank_n$campaign <- cut(bank_n$campaign,
                       breaks = c(1, 2, 3, 5, 10, 15, 20, max(bank_n$campaign)),
                       labels = c("1-2","2-3", "3-5", "5-10", "10-15", "15-20", "20+"))
bank_n$pdays <- cut(bank_n$pdays,
                    breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 28, max(bank_n$pdays)),
                    labels = c("0","1", "2", "3", "4", "5", "6", "7", "8", "9", "10-15", "15-20", "20-28", "999"))
bank_n$previous <- as.factor(bank_n$previous)
bank_n$emp.var.rate <- as.factor(bank_n$emp.var.rate)
bank_n$cons.price.idx <- cut(bank_n$cons.price.idx,
                             breaks = c(92,93, 93.5, 94, max(bank_n$cons.price.idx)),
                             labels = c("92-93", "93-93.5", "93.5-94", "94+"))
bank_n$cons.conf.idx <- cut(bank_n$cons.conf.idx,
                            breaks = c(-51, -45, -40, -35, max(bank_n$cons.conf.idx)),
                            labels = c("(-50)-(-45)", "(-45)-(-40)", "(-40)-(-35)", "(-35)+"))
bank_n$euribor3m <- cut(bank_n$euribor3m,
                        breaks = c(0,1, 2, 4, max(bank_n$euribor3m)),
                        labels = c("0-1", "1-2", "2-4", "4+"))
bank_n$nr.employed <- as.factor(bank_n$nr.employed)
# removing NAs
bank_n <- na.omit(bank_n)
# creating training and testing dataset from exisitng sample
indx <- createDataPartition(bank_n$y, p = 0.8, list = FALSE)
# creating lables for test and training data sets
bank_train_labels <- bank_n[indx,21]
bank_test_labels <- bank_n[- indx,21]
# removing dependent variable
bank_n <- bank_n[,-21]
bank_train <- bank_n[indx,]
bank_test <- bank_n[- indx,]
bank_classifier2 <- naiveBayes(bank_train, bank_train_labels, laplace = 1)
bank_test_pred2 <- predict(bank_classifier2, bank_test, type = "raw")
naive_pred = prediction(bank_test_pred2[,2],bank_test_labels)
naive_pref = performance(naive_pred,"tpr","fpr")

# KNN model
bank_k <- as.data.frame(scale(bank[c(1,11,12,13,14,16,17,18,19,20)]))
# creating training and testing dataset from exisitng sample
indx <- createDataPartition(bank$y, p = 0.8, list = FALSE)
bank_train_k <- bank_k[indx,]
bank_test_k <- bank_k[- indx,]
# creating lables for test and training data sets
bank_train_labels_k <- bank[indx,21]
bank_test_labels_k <- bank[- indx,21]
bank_test_predict.best <- class::knn(train = bank_train_k, test = bank_test_k, cl = bank_train_labels_k, k = 19, prob = TRUE)

prob <- attr(bank_test_predict.best, "prob")
prob <- 2*ifelse(bank_test_predict.best == "-1", 1-prob, prob) - 1

knn_pred = prediction(prob,bank_test_labels_k)
knn_pref = performance(knn_pred,"tpr","fpr")

# under sampling the data - 
table(bank_train$y)
bankTrain_under <- ovun.sample(y ~ ., data = bank_train, method = "under",N = 7424)$data
table(bankTrain_under$y)

# training the best models on new trainig upsampled data sets
# Decision tree
bank_JRip_under <- JRip(y ~ . , data = bankTrain_under)
bank_predict_JRip_under_cv <- predict(object = bank_JRip_under, newdata = bank_test)
confusionMatrix(bank_predict_JRip_under_cv, bank_test$y)
confusionMatrix(bank_predict_JRip_under_cv, bank_test$y, mode = "prec_recall")

bank_predict_JRip_under <- predict(object = bank_JRip_under, newdata = bank_test,  type = "prob")
JRip_pred_u = prediction(bank_predict_JRip[,2],bank_test$y)
JRip_pref_u = performance(JRip_pred,"tpr","fpr")


# ROC curve
plot(JRip_pref, col=1, lwd=3,avg= "threshold", main="ROC curve JRip DT vs SVM vs NB vs KNN vs Under sampling JRip DT",)
legend(0, 1, c("JRip","SVM", "NB", "KNN", "US_JRIP"), 1:5)
plot(svm_pref, col=2, lwd=3,add = TRUE)
plot(naive_pref, col=3, lwd=3, add=TRUE)
plot(knn_pref, col=4, lwd=3, add=TRUE)
plot(JRip_pref_u, col=5, lwd=3, add=TRUE)
abline(a=0,b=1,lwd=2,lty=2,col="grey")

#AUC
auc_Jrip <- performance(JRip_pred,"auc")
auc_Jrip <- unlist(slot(auc_Jrip, "y.values"))
auc_Jrip

auc_svm <- performance(svm_pred,"auc")
auc_svm <- unlist(slot(auc_svm, "y.values"))
auc_svm

auc_naive <- performance(naive_pred,"auc")
auc_naive <- unlist(slot(auc_naive, "y.values"))
auc_naive

auc_knn <- performance(knn_pred,"auc")
auc_knn <- unlist(slot(auc_knn, "y.values"))
auc_knn

auc_us_JRip <- performance(JRip_pred_u,"auc")
auc_us_JRip <- unlist(slot(auc_us_JRip, "y.values"))
auc_us_JRip

# comparision matrix
Model <- c("JRip DT", "SVM", "Naive Bayes", "KNN", "Under Sampling JRip DT")

# validation classification accuracy
model_auc <- c(auc_Jrip, auc_svm, auc_naive,auc_knn, auc_us_JRip)

metrics <- data.frame(Model, model_auc)  # data frame with above metrics

knitr::kable(metrics, digits = 5)


# visualize the Under Sampling JRip DT
# visulazing decision tree
bank_JRip_under

bank_JRip_under_ctreeall <- ctree(y ~ duration + emp.var.rate, data = bankTrain_under)
plot(bank_JRip_under_ctreeall, main = "High level decision tree")

