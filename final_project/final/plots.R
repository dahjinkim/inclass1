## Plots

rm(list=ls())

#read packages
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)


#read data
fullweekly <- read.csv("C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/group_work/final_project/fullweekly.csv")
fullweekly$week <- as.Date(fullweekly$week)


#full trend
full <- ggplot(data=fullweekly)+
  geom_point(mapping= aes(x=week, y=log(deathsum)))+
  geom_smooth(mapping = aes(x=week, y=log(deathsum)), method=loess) +
  geom_point(mapping=aes(x=week, y=log(committee), color="Committee"), alpha=0.5)+
  geom_smooth(mapping=aes(x=week, y=log(committee), color="Committee"), method=loess) +
  geom_point(mapping=aes(x=week, y=log(individual), color="Individual"), alpha=0.5)+
  geom_smooth(mapping=aes(x=week, y=log(individual), color="Individual"), method=loess) +
  scale_x_date(limits=c(as.Date("2020-01-01"),as.Date("2020-4-28" )))
full +  labs(title="Full Trend", y="Logged Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(name = "Contribution", values = c("Committee" = "darkgreen", "Individual" = "darkorange"), labels = c("Committee", "Individual"))


# all states
allstate <- ggplot(data=fullweekly)+
  geom_line(mapping= aes(x=week, y=log(deathsum)))+
  geom_line(mapping=aes(x=week, y=log(committee), color="Committee"))+
  geom_line(mapping=aes(x=week, y=log(individual), color="Individual"))+
  scale_x_date(limits=c(as.Date("2020-1-01"),as.Date("2020-4-28" )))+
  facet_wrap(~state)
allstate +  labs(title="Contribution and COVID19 Deaths (50 States)", y="Logged Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(name = "Contribution", values = c("Committee" = "darkgreen", "Individual" = "darkorange"), labels = c("Committee", "Individual"))


# individual state (eg. NY)
state1 <- ggplot(data=filter(fullweekly, state=="NY"))+
  geom_line(mapping= aes(x=week, y=log(deathsum)))+
  geom_line(mapping=aes(x=week, y=log(committee), color="Committee"))+
  geom_line(mapping=aes(x=week, y=log(individual), color="Individual"))+
  scale_x_date(limits=c(as.Date("2020-1-01"),as.Date("2020-4-28" )))+
  labs(title="Contribution and COVID19 Deaths (New York)", y="Logged Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_manual(name = "Contribution", values = c("Committee" = "darkgreen", "Individual" = "darkorange"), labels = c("Committee", "Individual"))


# we can also make an option to choose which contribution to see if we have time