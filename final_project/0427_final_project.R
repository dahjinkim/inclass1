# read in the data
# health data
library(readr)
library(dplyr)
library(stringr)

contrib_dir = "C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/group_work/final_project/itpas2.txt"
covid_dir = 'C:/Users/oyina/src/senior_2019-2020/spring_2020/political_data_science/group_work/final_project/daily.csv'
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
  filter(year != "2018")
  # mutate(date = as.Date(date, "%m/%d/%Y"))


covid = covid_og %>%
  mutate(year = str_sub(date, 1, 4)) %>%
  mutate(day = str_sub(date, 7, 8)) %>%
  mutate(month = str_sub(date, 5, 6))



# write web scrapper or use api for 

# contributions from individuals
# contributions from committess and to candidates and independent expenditures

# https://www.fec.gov/data/browse-data/?tab=bulk-data