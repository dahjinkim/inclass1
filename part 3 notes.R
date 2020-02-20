library(tidyverse)
mayors<-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/Mayors.csv")
tweets<-read_csv("Tweets.csv")
print(object.size(mayors), units="auto")
print(object.size(tweets), units="auto")

tweets<-rename(tweets, TwitterHandle=ScreenName)

mayors%>%
  count(TwitterHandle)%>%
  filter(n>1)

tweets<-tweets%>%
  left_join(select(mayors, TwitterHandle, LastName), by= "TwitterHandle")

 tweets%>%
  left_join(select(mayors, GenderMale, PartyRepublican, TwitterHandle, LastName, CityName), by= "TwitterHandle")


#count number of tweets per mayor
twitterCounts<-tweets%>%
  count(TwitterHandle)
mayors%>%
  left_join(count(tweets, TwitterHandle), by="TwitterHandle")
mayors%>%
  right_join(count(tweets, TwitterHandle), by="TwitterHandle")
mayors%>%
  inner_join(count(tweets, TwitterHandle), by="TwitterHandle")
mayors%>%
  full_join(count(tweets, TwitterHandle), by="TwitterHandle")

duplicateMayor<-filter(mayors,TwitterHandle%in%c("robertgarcialb","rodhiggins2017") )
duplicateTweets<-filter(tweets,TwitterHandle%in%c("robertgarcialb","rodhiggins2017") )

duplicateTweets%>%
  full_join(select(duplicateTweets, TwitterHandle, LastName), by=TwitterHandle)

mentions <-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/TwitterMentions.csv")

mentions<-rename(mentions, TwitterHandle=MayorHandle)
mentions%>% 
  count(TwitterHandle)
mentions<-mentions%>%
  left_join(count(tweets,TwitterHandle), by="TwitterHandle")
#combine all tweets into one database, keeping overlapping columns
combine<-inner_join(tweets, mentions)

#check to see if any mayors are in the mentions list

mayormentions<-semi_join(mentions, mayors, by="TwitterHandle")


  
aTweet<-tweets[1,]$Text
str_length(aTweet)
words<-str_split(aTweet, pattern=" ")
str_c(unlist(words), collapse = " ")

krewson<-filter(tweets, TwitterHandle=="lydakrewson" )

krewsonwords<-str_split(krewson$Text, pattern=" ")

wordlist<-NULL
for(i in krewsonwords){
  wordlist<-c(wordlist, length(i))
}
mean(wordlist)


onestring<-unlist(krewsonwords)
uniquewords<- unique(onestring)

firstfive<-substr(onestring, 1,5)

length(unique(firstfive))

sum(str_detect(onestring, "polic"))
sum(str_detect(onestring, "police"))
sum(str_detect(onestring, "http"))

lowercasewords<-str_to_lower(krewson$Text)

sum(str_detect(lowercasewords, "polic"))
sum(str_detect(lowercasewords, "police"))
sum(str_detect(lowercasewords, "http"))
str_subset(lowercasewords, "black lives")
str_subset(lowercasewords, "police")
