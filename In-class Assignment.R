##### In-class Assignment
##### Team: Alex Newman, Diva Harsoor, Oyin

#preparation
library(tidyverse)
library(stringr)
mayors<-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/Mayors.csv")
tweets<-read_csv("Tweets.csv")

##### 1 
##### Using this dataset, come up with your own approach for identifying tweets 
##### that mentions: Police, cops, and synonyms & Black lives matters protest movements

tweets<-rename(tweets, TwitterHandle=ScreenName)

#identifying tweets for Police, cops
police.tweets <- grep("polic", tweets$Text, ignore.case=TRUE, perl=TRUE)
police.tweets <- tweets[police.tweets, ]

#identifying tweets for Black lives matters
blacklives.tweets <- grep("black live", tweets$Text, ignore.case=TRUE, perl=TRUE)
blacklives.tweets <- tweets[blacklives.tweets, ]

#? how to identify multiple patterns ?



##### 2
##### For each mayor in the dataset, what number of tweets match those criteria.

#match the TwitterHandle with mayors data set
police.mayors <- mayors %>%
  left_join(count(police.tweets, TwitterHandle), by = "TwitterHandle")
blacklives.mayors <- mayors %>%
  left_join(count(blacklives.tweets, TwitterHandle), by = "TwitterHandle")

#? how to retain subset of columns ?
# change column names


##### 3
##### Using the mayors data from last time, 
##### show how these summaary statistics relate (if it relates) 
##### to the population size of the city. (Plot)


