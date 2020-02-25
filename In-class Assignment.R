##### In-class Assignment
##### Team: Alex Newman, Diva Harsoor, Oyin, Jin
rm(list=ls())

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
police.tweets <- grep("polic| cop | cops|law enforc|bluelives|officer", tweets$Text, ignore.case=TRUE, perl=TRUE)
police.tweets <- tweets[police.tweets, ]
#removing policy/policies
other <- grep("policy|policies", police.tweets$Text, ignore.case = TRUE, perl = TRUE)
police.tweets <- police.tweets[-other, ]

#identifying tweets for Black lives matters
blacklives.tweets <- grep("black live|blacklives|blm|#ferguson| ferguson", tweets$Text, ignore.case=TRUE, perl=TRUE)
blacklives.tweets <- tweets[blacklives.tweets, ]



##### 2
##### For each mayor in the dataset, what number of tweets match those criteria.

#match the TwitterHandle with mayors data set
police.mayors <- mayors %>%
  left_join(count(police.tweets, TwitterHandle), by = "TwitterHandle")
blacklives.mayors <- mayors %>%
  left_join(count(blacklives.tweets, TwitterHandle), by = "TwitterHandle")

#number of police tweets per mayors
police.mayors <- police.mayors %>% mutate(Polcount = replace_na(n, 0))
police.mayors <- select(police.mayors, FullName, Polcount, Population)
police.mayors <- drop_na(police.mayors, c(FullName, Population))

#number of BLM tweets per mayors
blacklives.mayors <- blacklives.mayors %>% mutate(BLMcount = replace_na(n, 0))
blacklives.mayors <- select(blacklives.mayors, FullName, BLMcount, Population)
blacklives.mayors <- drop_na(blacklives.mayors, c(FullName, Population))

#combining the data set
mayor.tweet.data <- blacklives.mayors %>%
  left_join(police.mayors, by = "FullName") %>%
  mutate(Population = Population.x) %>%
  select(FullName, BLMcount, Polcount, Population)


##### 3
##### Using the mayors data from last time, 
##### show how these summaary statistics relate (if it relates) 
##### to the population size of the city. (Plot)

mayortweetplot <- ggplot(data=mayor.tweet.data) + 
  geom_point(mapping = aes(x = Population, y = Polcount, color = "Police")) +
  geom_point(mapping = aes(x = Population, y = BLMcount, color = "BLM")) +
  labs(x = "Population", y = "Tweet Counts", title="City Population and Tweets") +
  scale_color_manual(name = "Tweet", values = c("Police" = "blue", "BLM" = "red"), 
                     labels = c("BLM", "Police"))


mayortweetplot

