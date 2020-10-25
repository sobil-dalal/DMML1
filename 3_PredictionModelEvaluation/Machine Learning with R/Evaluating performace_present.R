setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/3_PredictionModelEvaluation/Machine Learning with R/")

# reading the csv
sms_results <- read.csv("sms_results.csv")

# head of file
head(sms_results)

# checking values near the probablity region of 0.40 and .60
head(subset(x = sms_results, subset = prob_spam > 0.40 & prob_spam < 0.60))

# checking values with actual and predict type are diff
head(subset(x = sms_results, subset = actual_type != predict_type))

# creating a confusion matrix
table(sms_results$actual_type, sms_results$predict_type)
# TP = 1203, TN = 152, FP = 31, FN = 4

# better way to create confusion matrix is by gmodels library
library(gmodels)
gmodels::CrossTable(sms_results$actual_type, sms_results$predict_type)

# beyond accuracy : other measures using caret library
library(caret)
caret::confusionMatrix(reference = sms_results$actual_type, data = sms_results$predict_type, positive = "spam")

# calculate kappa value individual 
library(vcd)
vcd::Kappa(table(sms_results$actual_type, sms_results$predict_type))

# calculating sensitivity
caret::sensitivity(reference = sms_results$actual_type, data = sms_results$predict_type, positive = "spam")
caret::specificity(reference = sms_results$actual_type, data = sms_results$predict_type, negative = "ham")

# visualization using ROC
library(ROCR)
# calculating prediction values - for ROC
pred <- ROCR::prediction(predictions = sms_results$prob_spam, labels = sms_results$actual_type)

# creating a performance object using prediction object
pref <- ROCR::performance(prediction.obj = pred, measure = "tpr", x.measure = "fpr")

# plotting the preformace object
plot(pref, main = "ROC curve for SMS spam filter", col = "blue", lwd = 3)
# creating a line in slope- interscept form
abline(a = 0, b = 1, lwd = 2, lty  = 2)

# checking area under the curve value
# performace object
pref.auc <- ROCR::performance(prediction.obj = pred, measure = "auc")

# using ulist function to evaluate the auc value from performace object
unlist(pref.auc@y.values)
# auc = .9835 i.e 98.35 ~ > 90% --> oustanding