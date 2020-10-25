setwd('/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/4_RegressionModelsClassificationKNN/Rscript/')
remove(list = ls())

library(htmltab)

url <- "https://en.wikipedia.org/wiki/Game_of_Thrones"
seasons <- htmltab::htmltab(doc = url, which = 2)
row.names(seasons) <- c(1:8)

critics <- htmltab::htmltab(doc = url, which = 3)

#  Tasks
#1 Fix the row names
row.names(critics) <- c(1:8)
#2 : Encode the Season attibute in both data.frames such that both are factors, and such that they both use the same level labels. We need them to be represented in the same way, as we are going to use this column as a reference point to merge the two tables into one table.
seasons$Season <- factor(seasons$Season, levels = c("Season 1" , "Season 2", "Season 3" , "Season 4", "Season 5", "Season 6", "Season 7", "Season 8"), labels = c(1:8))
critics$Season <- factor(critics$Season)

# merge the df to 1
GoTWikipedia <- merge(seasons, critics, by = 'Season')


# using the API from the movie DB
url <- "https://api.themoviedb.org/3/tv/1399?api_key=dde2c2f722a573a1f976859991500a48"
library(rjson)
GotJson <- jsonlite::fromJSON(txt = url, flatten = TRUE)

key <- "dde2c2f722a573a1f976859991500a48"
url <- paste0("https://api.themoviedb.org/3/tv/1399/season/1", "?api_key=", key) 
GoTJsonSeason1 <- jsonlite::fromJSON(url, flatten = TRUE)
url <- paste0("https://api.themoviedb.org/3/tv/1399/season/2", "?api_key=", key) 
GoTJsonSeason2 <- jsonlite::fromJSON(url, flatten = TRUE)
url <- paste0("https://api.themoviedb.org/3/tv/1399/season/3", "?api_key=", key) 
GoTJsonSeason3 <- jsonlite::fromJSON(url, flatten = TRUE)
url <- paste0("https://api.themoviedb.org/3/tv/1399/season/4", "?api_key=", key) 
GoTJsonSeason4 <- jsonlite::fromJSON(url, flatten = TRUE)
url <- paste0("https://api.themoviedb.org/3/tv/1399/season/5", "?api_key=", key) 
GoTJsonSeason5 <- jsonlite::fromJSON(url, flatten = TRUE)
url <- paste0("https://api.themoviedb.org/3/tv/1399/season/6", "?api_key=", key) 
GoTJsonSeason6 <- jsonlite::fromJSON(url, flatten = TRUE)
url <- paste0("https://api.themoviedb.org/3/tv/1399/season/7", "?api_key=", key) 
GoTJsonSeason7 <- jsonlite::fromJSON(url, flatten = TRUE)
url <- paste0("https://api.themoviedb.org/3/tv/1399/season/8", "?api_key=", key) 
GoTJsonSeason8 <- jsonlite::fromJSON(url, flatten = TRUE)

movieDBRatings <- c()
movieDBRatings[1] <- mean(GoTJsonSeason1$episodes$vote_average) 
movieDBRatings[2] <- mean(GoTJsonSeason2$episodes$vote_average) 
movieDBRatings[3] <- mean(GoTJsonSeason3$episodes$vote_average) 
movieDBRatings[4] <- mean(GoTJsonSeason4$episodes$vote_average) 
movieDBRatings[5] <- mean(GoTJsonSeason5$episodes$vote_average) 
movieDBRatings[6] <- mean(GoTJsonSeason6$episodes$vote_average) 
movieDBRatings[7] <- mean(GoTJsonSeason7$episodes$vote_average)
movieDBRatings[8] <- mean(GoTJsonSeason8$episodes$vote_average)

#Tasks
# 1. Make the movieDBRatings vector a % score (multiply by 10), and round them
movieDBRatings <- round(movieDBRatings * 10)

# 2. Add averages into the Wikipedia data.frame GoTWikipedia 
GoTWikipedia$movieDBRatings <- movieDBRatings

# Task Make two new attributes RT and Critic that are the first 2 characters of the corresponding GoTWikipedia data.frame and cast them to numerics
GoTWikipedia$RT = as.numeric(substr(x = GoTWikipedia$`Critical response >> Rotten Tomatoes`,start = 1, stop = 2))
GoTWikipedia$Critic = as.numeric(substr(x = GoTWikipedia$`Critical response >> Metacritic`, start = 1, stop = 2))

# plot the ratings
library(ggplot2)
library(ggthemes)
library(reshape)
data <- data.frame(GoTWikipedia[,c("Season", "RT", "Critic", "movieDBRatings")])
data$Season <- as.numeric(data$Season)

Molten <- melt(data = data, id.vars = "Season")
ggplot(data = Molten, aes(x= Season, y= value, colour = variable)) + geom_line() + scale_x_continuous(breaks=c(1:7)) + ylab("Average review %")

getGender <- function(seasonJSON) { 
  gender <- c()
  for (i in 1:length(seasonJSON)) { 
    tmp <- seasonJSON[[i]]
    gender <- c(gender, tmp$gender)
  }
  return(gender) 
}

# Tasks
#1. Repeat for the other 5 seasons making a factor vector s3gender, . . . , s7gender
s1gender <- factor(getGender(GoTJsonSeason1$episodes$guest_stars))
s2gender <- factor(getGender(GoTJsonSeason2$episodes$guest_stars))
s3gender <- factor(getGender(GoTJsonSeason3$episodes$guest_stars))
s4gender <- factor(getGender(GoTJsonSeason4$episodes$guest_stars))
s5gender <- factor(getGender(GoTJsonSeason5$episodes$guest_stars))
s6gender <- factor(getGender(GoTJsonSeason6$episodes$guest_stars))
s7gender <- factor(getGender(GoTJsonSeason7$episodes$guest_stars))
s8gender <- factor(getGender(GoTJsonSeason8$episodes$guest_stars))

# 2. Compute the number of each gender and instantiate attibutes in our main GoT data.frame corresponding
#o 0 (unknown), 1 (female), 2 (male)

# creating the list containg all season genders
gender.season <- list(s1gender, s2gender, s3gender, s4gender, s5gender, s6gender, s7gender, s8gender)
# calculating total size of gender per season
gender.male <- unlist(lapply(X = gender.season , FUN = function(x){
    length(x[x==2])
  }))
gender.female <- unlist(lapply(X = gender.season , FUN = function(x){
  length(x[x==1])
}))
gender.unknown <- unlist(lapply(X = gender.season , FUN = function(x){
  length(x[x==0])
}))
totalExtras <- unlist(lapply(X = gender.season , FUN = function(x){
  length(x)}))
# Adding to main data frame
GoTWikipedia$maleExtras <- gender.male
GoTWikipedia$femaleExtras <- gender.female
GoTWikipedia$unknownGenderExtras <- gender.unknown
GoTWikipedia$totalExtras <- totalExtras

#3. Plot Gender against Season to work out how the gender of extras / guest stars changes over the seasons
data <- data.frame(GoTWikipedia[, c("Season", "maleExtras", "femaleExtras", "unknownGenderExtras")])
data$Season <- as.numeric(data$Season)
Molten <- melt(data = data, id.vars = "Season")
ggplot(data = Molten, aes(x = Season, y = value, colour = variable)) + geom_line() + scale_x_continuous(breaks=c(1:8)) + ylab("Gender of extras (num)")


# To use this later in exercise
save(GoTWikipedia, file="GoTWikipedia.RData") 
save(GoTJsonSeason1, file="GoTJsonSeason1.RData") 
save(GoTJsonSeason2, file="GoTJsonSeason2.RData") 
save(GoTJsonSeason3, file="GoTJsonSeason3.RData") 
save(GoTJsonSeason4, file="GoTJsonSeason4.RData") 
save(GoTJsonSeason5, file="GoTJsonSeason5.RData") 
save(GoTJsonSeason6, file="GoTJsonSeason6.RData") 
save(GoTJsonSeason7, file="GoTJsonSeason7.RData")
save(GoTJsonSeason8, file="GoTJsonSeason8.RData")





# Exercise
# 1. Plot gender distributions by season





