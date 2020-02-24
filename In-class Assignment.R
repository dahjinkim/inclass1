##### In-class Assignment
##### Team: Alex Newman, Diva Harsoor, Oyin, Jin

#preparation
library(tidyverse)
library(stringr)
library(ggplot2)
mayors<-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/Mayors.csv")
tweets<-read_csv("Tweets.csv")
my.tweets <- tweets

##### 1 
##### Using this dataset, come up with your own approach for identifying tweets 
##### that mentions: Police, cops, and synonyms & Black lives matters protest movements

tweets<-rename(tweets, TwitterHandle=ScreenName)

#identifying tweets for Police, cops
police.tweets <- grep("polic|cop|law enforc|bluelives|officer", tweets$Text, ignore.case=TRUE, perl=TRUE)
police.tweets <- tweets[police.tweets, ]

#identifying tweets for Black lives matters
blacklives.tweets <- grep("black live|blacklives|blm", tweets$Text, ignore.case=TRUE, perl=TRUE)
blacklives.tweets <- tweets[blacklives.tweets, ]



##### 2
##### For each mayor in the dataset, what number of tweets match those criteria.

#match the TwitterHandle with mayors data set
police.mayors <- mayors %>%
  left_join(count(police.tweets, TwitterHandle), by = "TwitterHandle")
blacklives.mayors <- mayors %>%
  left_join(count(blacklives.tweets, TwitterHandle), by = "TwitterHandle")

police.mayors <- police.mayors %>% mutate(Count = replace_na(n, 0))
police.mayors <- select(police.mayors, FullName, Count, Population)
police.mayors <- drop_na(police.mayors, c(FullName, Population))

blacklives.mayors <- blacklives.mayors %>% mutate(Count = replace_na(n, 0))
blacklives.mayors <- select(blacklives.mayors, FullName, Count, Population)
blacklives.mayors <- drop_na(blacklives.mayors, c(FullName, Population))



##### 3
##### Using the mayors data from last time, 
##### show how these summaary statistics relate (if it relates) 
##### to the population size of the city. (Plot)

policetweetplots <- ggplot(data=police.mayors) + geom_point(mapping = aes(x = Population, y = Count))
BLMtweetplots <- ggplot(data=blacklives.mayors) + geom_point(mapping = aes(x = Population, y = Count))

policetweetplots
BLMtweetplots
