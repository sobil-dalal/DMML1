remove(list = ls ())
setwd('/Users/sobil/Documents/MSC/Sem 1/Data Mining & Machine Learning/4_RegressionModelsClassificationKNN/Rscript/')
load("GoTWikipedia.RData")
load("GoTJsonSeason1.RData")
load("GoTJsonSeason2.RData")
load("GoTJsonSeason3.RData")
load("GoTJsonSeason4.RData") 
load("GoTJsonSeason5.RData")
load("GoTJsonSeason6.RData")
load("GoTJsonSeason7.RData")
load("GoTJsonSeason8.RData")

# Exercises
# 1. Plot gender distributions by season
getGender <- function(seasonJSON) { 
  gender <- list()
  #this time we're making a list, not a vector
  for (i in 1:length(seasonJSON)) { 
    gender[[i]] <- seasonJSON[[i]]$gender
  }
  gender.male <- unlist(lapply(gender, FUN = function(x) { 
      length(x[x == 2])
  }))
  gender.female <- unlist(lapply(gender, FUN = function(x) { 
    length(x[x == 1])
  }))
  gender.unknown <- unlist(lapply(gender, FUN = function(x) { 
    length(x[x == 0])
  }))
  gender.total <- unlist(lapply(gender, FUN = function(x) { 
    length(x)
  }))
  gender <- data.frame(gender.male, gender.female, gender.unknown, gender.total)
  #and we want to return a dataframe
  return(gender) 
}

s1genderByEpisode <- getGender(GoTJsonSeason1$episodes$guest_stars)
s2genderByEpisode <- getGender(GoTJsonSeason2$episodes$guest_stars)
s3genderByEpisode <- getGender(GoTJsonSeason3$episodes$guest_stars)
s4genderByEpisode <- getGender(GoTJsonSeason4$episodes$guest_stars)
s5genderByEpisode <- getGender(GoTJsonSeason5$episodes$guest_stars)
s6genderByEpisode <- getGender(GoTJsonSeason6$episodes$guest_stars)
s7genderByEpisode <- getGender(GoTJsonSeason7$episodes$guest_stars)
s8genderByEpisode <- getGender(GoTJsonSeason8$episodes$guest_stars)

s1genderByEpisode$Season <- 1
s1genderByEpisode$Episode <- c(1:nrow(s1genderByEpisode))
s2genderByEpisode$Season <- 2
s2genderByEpisode$Episode <- c(1:nrow(s2genderByEpisode))
s3genderByEpisode$Season <- 3
s3genderByEpisode$Episode <- c(1:nrow(s3genderByEpisode))
s4genderByEpisode$Season <- 4
s4genderByEpisode$Episode <- c(1:nrow(s4genderByEpisode))
s5genderByEpisode$Season <- 5
s5genderByEpisode$Episode <- c(1:nrow(s5genderByEpisode))
s6genderByEpisode$Season <- 6
s6genderByEpisode$Episode <- c(1:nrow(s6genderByEpisode))
s7genderByEpisode$Season <- 7
s7genderByEpisode$Episode <- c(1:nrow(s7genderByEpisode))
s8genderByEpisode$Season <- 8
s8genderByEpisode$Episode <- c(1:nrow(s8genderByEpisode))


gender <- rbind(s1genderByEpisode, s2genderByEpisode, s3genderByEpisode, s4genderByEpisode, s5genderByEpisode, s6genderByEpisode,
                s7genderByEpisode, s8genderByEpisode)


par(mfrow = c(2,2))

boxplot(gender$gender.male ~ gender$Season, xlab="Season", ylab="Number of Extras", main="Male Extras by Season")
boxplot(gender$gender.female ~ gender$Season, xlab="Season", ylab="Number of Extras", main="Female Extras by Season")
boxplot(gender$gender.unknown ~ gender$Season, xlab="Season", ylab="Number of Extras", main="Extras (unknown gender) by Season")
boxplot(gender$gender.total ~ gender$Season, xlab="Season", ylab="Number of Extras", main="Extras by Season")


# Question 2: Is there a relationship between the number of (fe)male extras and the Season ratings?
library(ggplot2)
library(ggthemes)
library(reshape)
data <- data.frame(GoTWikipedia[, c("RT", "Critic", "movieDBRatings", "maleExtras")]) 
Molten <- melt(data, id.vars = "maleExtras")
ggplot(Molten, aes(x = maleExtras, y = value, colour = variable)) + geom_line() + ylab("Review %") + xlab("No. of Male Extas")
ggplot(Molten, aes(x = maleExtras, y = value, colour = variable)) + geom_point() + ylab("Review %") + xlab("No. of Male Extas")


data <- data.frame(GoTWikipedia[, c("RT", "Critic", "movieDBRatings", "femaleExtras")])
Molten <- melt(data, id.vars = "femaleExtras")
ggplot(Molten, aes(x = femaleExtras, y = value, colour = variable)) + geom_point() +
  ylab("Review %") + xlab("No. of Female Extas")

# Question 3: Do more extras tend to result in better ratings?
data <- data.frame(GoTWikipedia[, c("RT", "Critic", "movieDBRatings", "totalExtras")]) 
Molten <- melt(data, id.vars = "totalExtras")
ggplot(Molten, aes(x = totalExtras, y = value, colour = variable)) + geom_point() +
  ylab("Review %") + xlab("No. of Extas")


# Question 4: What are the most common words in season / episode overviews?
sOverviews <- c( GoTJsonSeason1$overview, GoTJsonSeason2$overview, GoTJsonSeason3$overview, GoTJsonSeason4$overview, GoTJsonSeason5$overview, GoTJsonSeason6$overview, GoTJsonSeason7$overview, GoTJsonSeason8$overview)
eOverviews <- c( GoTJsonSeason1$episodes$overview, GoTJsonSeason2$episodes$overview, GoTJsonSeason3$episodes$overview, GoTJsonSeason4$episodes$overview, GoTJsonSeason5$episodes$overview, GoTJsonSeason6$episodes$overview, GoTJsonSeason7$episodes$overview, GoTJsonSeason8$episodes$overview)

library(tm)
seasonOverviews <- tm::Corpus(tm::VectorSource(sOverviews))
episodeOverviews<- tm::Corpus(tm::VectorSource(eOverviews))
summary(seasonOverviews)
tm::inspect(seasonOverviews)

# Convert the text to lower case (no need to differentiate for
# example between "Me" and "me")
seasonOverviews <- tm::tm_map(seasonOverviews, content_transformer(tolower)) 
episodeOverviews <- tm::tm_map(episodeOverviews, content_transformer(tolower))

# Remove numbers: not needed
seasonOverviews <- tm_map(seasonOverviews, removeNumbers)
episodeOverviews <- tm_map(episodeOverviews, removeNumbers)
# Remove english common stopwords
seasonOverviews <- tm_map(seasonOverviews, removeWords, stopwords("english"))
episodeOverviews <- tm_map(episodeOverviews, removeWords, stopwords("english"))

# Remove punctuation
seasonOverviews <- tm_map(seasonOverviews, removePunctuation)
episodeOverviews <- tm_map(episodeOverviews, removePunctuation)

# Eliminate extra white spaces
seasonOverviews <- tm_map(seasonOverviews, stripWhitespace)
episodeOverviews <- tm_map(episodeOverviews, stripWhitespace)

#  build a term-document matrix
dtmSeason <- TermDocumentMatrix(seasonOverviews)
dtmEpisodes <- TermDocumentMatrix(episodeOverviews)

dtmSeason

# Compute which column has the highest sum, or
m <- as.matrix(dtmSeason)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

m <- as.matrix(dtmEpisodes)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# WORD - CLOUD
library(wordcloud)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



# Question 5: Which season has the most positively phrased overview?
