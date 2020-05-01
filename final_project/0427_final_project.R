# read in the data
# health data
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

contrib_dir = "itpas2.txt"
covid_dir = 'daily.csv'
covid_og = read.csv(covid_dir)
contrib_og = read.delim(contrib_dir, sep = '|', header = FALSE)
contrib_og = data.frame(contrib_og)

contrib = contrib_og %>%
  rename(id = V1, type = V4, state = V10, zip = V11, amount = V15, cand_id = V17, date = V14) %>%
  select(id, type, state, zip, amount, cand_id, date) %>%
  mutate(date = as.character(date)) %>%
  mutate(year = str_sub(date, -4, -1)) %>%
  mutate(day = str_sub(date, -6, -5)) %>%
  mutate(month = str_sub(date, -8, -7)) %>%
  filter(year != "2018") %>%
  mutate(month = paste0("0", month)) %>%
  mutate(month = str_sub(month, -2, -1)) %>%
  mutate(date_ = as.Date(paste0(year, "-", month, "-", day))) #maybe?
  # mutate(date_ = as.Date(date, "%m/%d/%Y"))


covid = covid_og %>%
  mutate(year = str_sub(date, 1, 4)) %>%
  mutate(day = str_sub(date, 7, 8)) %>%
  mutate(month = str_sub(date, 5, 6)) %>%
  mutate(date_ = as.Date(paste0(year, "-", month, "-", day))) #maybe?


# create date range vector
start_date = as.Date("2019-01-01")
date_ranges = rep(NA, 106)
for (i in 1:106) {
  date_ranges[i] = as.character(as.Date(start_date))
  start_date = start_date + 7
}
date_ranges = as.Date(date_ranges)


# add row to covid data for week
covid_week = rep(NA, nrow(covid))
for (row in 1:nrow(covid)) {
  for (date in 1:length(date_ranges)) {
    if (covid$date_[row] <= date_ranges[date]) {
      # print(covid$date[row])
      # print(date_ranges[date])
      covid_week[row] = as.character(as.Date(date_ranges[date]))
      break
    }
  }
}
covid = covid %>%
  mutate(week = covid_week)

# add row to contrib data for week
contrib_week = rep(NA, nrow(contrib))
for (row in 1:nrow(contrib)) {
  for (date in 1:length(date_ranges)) {
    if (contrib$date_[row] <= date_ranges[date]) {
      # print(covid$date[row])
      # print(date_ranges[date])
      contrib_week[row] = as.character(as.Date(date_ranges[date]))
      break
    }
  }
}

stateabbsdc<-c(state.abb,"DC")

print(contrib_week)
contrib = contrib %>%
  mutate(week = contrib_week)

weeklycontribs<-contrib%>%
  group_by(state,week)%>%
  summarise(sum(amount))%>%
  rename(amountsum= `sum(amount)`)
  
weeklycontribs<-weeklycontribs%>%
  filter(state%in%stateabbsdc)

#weeklycovid<-covid%>%
  #filter(state%in% stateabbsdc)

weeklycovid<-covid%>%
  group_by(state,week)%>%
  summarise(sum(deathIncrease))%>%
  rename(deathsum= `sum(deathIncrease)`)

fullweekly<-full_join(weeklycovid,weeklycontribs, by= c("state","week"))

fullweekly<-fullweekly%>%
  mutate(week=as.Date(week))

ggplot(data=filter(fullweekly, state=="NY") )+
  geom_line(mapping= aes(x=week, y=log(deathsum)))+
  geom_line(mapping=aes(x=week, y=log(amountsum), color="red"))+
  scale_x_date(limits=c(as.Date("2020-3-10"),as.Date("2020-4-28" )))#+
  #facet_wrap(~state)
  
# write web scrapper or use api for 

# contributions from individuals
# contributions from committess and to candidates and independent expenditures

# https://www.fec.gov/data/browse-data/?tab=bulk-data