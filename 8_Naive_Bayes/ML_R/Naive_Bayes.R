setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/8_Naive_Bayes/ML_R/")
remove(list = ls())

library(tm)
library(wordcloud)
library(e1071)
library(caret)

# Step 1 : Collect data - 
#/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/8_Naive_Bayes/ML_R/sms_spam.csv
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)





# Step 2: Explore and prepare the data
# explore
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
table(sms_raw$type)


# Data Prepration : clean & standardizing text
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# Document Term matrix
sms_dtm <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = function(x) { removeWords(x,stopwords())},
  removePunctuation = TRUE,
  stemming = TRUE
))


# Data Prepration : creating training and test datasets
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_train_labels <- sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4170:5559,]$type

# word cloud
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3,0.5), random.order = FALSE)
wordcloud(ham$text, max.words = 40, scale = c(3,0.5), random.order = FALSE)


# Data Prepration : creating indicator features for frequent words
sms_freq_words <- findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)

sms_dtm_freq_train <- sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]

# naive bayes works with factors, so converting the numerical columns to factors
convert_counts <- function(x){
  x <- ifelse(x>0 , "Yes", "No")
}

# matrices train/test
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)




# Step 3 : Train the model
sms_classifier <- naiveBayes(sms_train, sms_train_labels)



# Step 4 : Evaluate the model
sms_test_pred <- predict(sms_classifier, sms_test)
caret::confusionMatrix(sms_test_pred, sms_test_labels)


# Step 5 : Improving the model
sms_classifier <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred <- predict(sms_classifier, sms_test)
caret::confusionMatrix(sms_test_pred, sms_test_labels)