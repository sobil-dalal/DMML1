setwd("/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/Project/Kc_house/")
remove(list = ls())

library(psych)

# reading the raw csv file
kc_house <- read.csv("kc_house_data.csv", stringsAsFactors = FALSE)

# exploratory analysis 
head(kc_house)
str(kc_house)
summary(kc_house)



# cleaning & removing the unrelated and insignificant data
# removing id
kc_house <- kc_house[,-c(1)]

# coverting the date field type from chr to date
kc_house$date <- as.Date(kc_house$date, format = "%Y%m%d")
summary(kc_house$date)
# Range => Min date - "2014-05-02" to Max = "2015-05-27"
# Almost for the same duration of time, so the sample is unbiased
# remove date
kc_house <- kc_house[,-c(1)]

# removing the lat, long, zipcode
kc_house <- kc_house[,-c(15,16,17)]

# sqft_living, sqft_lot are related and can calculate sqft_living15, sqft_lot15 resp.
kc_house <- kc_house[,-c(15,16)]

# converting the waterfront to factors (0,1)
table(kc_house$waterfront)
kc_house$waterfront <- as.factor(kc_house$waterfront)


# cheking NAs
apply(X = kc_house,MARGIN =  2, FUN = function(col) any(is.na(col))) # no NAs

# exploring each colmuns
table(kc_house$bedrooms) # 33 bedroom seems to be a influential point
kc_house <- kc_house[kc_house$bedrooms != 33,]

table(kc_house$bathrooms)

table(kc_house$view)

row.names(kc_house) <- 1:21612