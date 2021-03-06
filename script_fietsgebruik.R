# clean workspace
rm(list=ls())

# load libraries
library(tidyverse)

library(sf)
library(tmap)

library(lubridate)

# LOAD DATA IN AND PREP ----------------------

# set path
path1 <- c("D:/NR Win folders/Documents/A folder/Data Science/R/Case_Locus")
setwd(path1)
getwd()

# data set of station CB2105!!
# read in and explore historic data from fiets tellingen data set of station CB2105
api_request <- c("?request=history&featureID=CB2105&startDate=20190304&endDate=20190310&outputFormat=csv")
api_url <- "https://data-mobility.brussels/bike/api/counts/"
week0403 <- read_csv(paste0(api_url, api_request))
head(week0403)

api_request <- c("?request=history&featureID=CB2105&startDate=20190225&endDate=20190303&outputFormat=csv")
week2502 <- read_csv(paste0(api_url, api_request))
head(week2502)

week0403 %>%
  glimpse() %>%
  summary()

weeks <- rbind(week0403, week2502)
weeks <- weeks %>%
  arrange(Date, `Time gap`)

weeks$week <- week(weeks$Date)
weeks$week_day <- wday(weeks$Date, label = TRUE)

# correct for lubridate week function starting on sunday, not correct for week 1?
for(i in 1:nrow(weeks)) {
  if (weeks$week_day[i] == "ma") {
    weeks$week[i] <- c(week(weeks$Date[i])+1)
    }
  else {
    weeks$week[i] <- c(week(weeks$Date[i]))
  }
}

weeks <- weeks %>%
  arrange(Date, `Time gap`)

weeks$week_day <- factor(weeks$week_day,levels(weeks$week_day)[c(2:7,1)])

unique(weeks$Date)
week(unique(weeks$Date))

# VISUAL EXPLORATION WITH SOME PLOTS --------------------------
# line plot drukte per dag in week 9
ggplot(filter(weeks, week == 9)) +
  geom_line(aes(x = `Time gap`, y = Count, color = as.factor(Date)))

# bar plot drukte in week 9, Time gap omgevormd naar uren van de dag
ggplot(filter(weeks, week == 9)) +
  geom_bar(stat = "identity", aes(x = `Time gap`*15/60, y = Count, color = as.factor(week))) +
  facet_wrap(~ as.factor(week_day))

# plot drukte over de weken van 04/03/2019 heen, pas op want stacked als meerdere weken
ggplot(filter(weeks, week == 10)) +
  geom_bar(stat = "identity", aes(x = `Time gap`*15/60, y = Count, color = as.factor(week))) +
  facet_wrap(~ week_day, ncol = 7) +
  xlab("Hours")

# plot van drukte per dag per week, pieken in de ochtend en avond
ggplot(weeks) +
  geom_bar(stat = "identity", aes(x = `Time gap`*15/60, y = Count, color = as.factor(week)), width = 0.25) +
  facet_grid(as.factor(week) ~ week_day)

# COUNT PER HOUR -----------------------

# transform counts per quarter to per hour
weeks$hour <- floor(weeks$`Time gap` * 15 / 60)

count_per_hour <- weeks %>%
  group_by(Date, week, week_day, hour) %>%
  summarise(count = sum(Count),
            avg_speed = sum(`Average speed` * Count) / sum(Count))


# plot van drukte per uur per dag per week, pieken in de ochtend en avond
ggplot(count_per_hour) +
  geom_bar(stat = "identity", aes(x = hour, y = count, color = as.factor(week))) +
  facet_grid(as.factor(week) ~ week_day)

# Does average speed depend on the time in the day? No real visable impact on this graph.
ggplot(count_per_hour) +
  geom_bar(stat = "identity", aes(x = hour, y = avg_speed, color = as.factor(week))) +
  facet_grid(as.factor(week) ~ week_day)


# UNDER CONSTRUCTION-------------------------------


# api_request <- c("?request=history&featureID=_A&startDate=20190304&endDate=20190310&outputFormat=csv")
# api_url <- "https://data-mobility.brussels/bike/api/counts/"
# test_csv2 <- read_csv(paste0(api_url, api_request))
# head(test_csv2)
# 
# test_csv2 %>%
#   glimpse() %>%
#   summary()
# 
# 
# 
# read in telpaal devices dataset
test_csv <- read_csv("http://data-mobility.brussels/geoserver/bm_bike/wfs?service=wfs&version=1.1.0&request=GetFeature&typeName=bm_bike%3Art_counting&outputFormat=csv")
head(test_csv)

# the year counts seem unstable, sometimes an NA value is given. Thus better to use saved file.
test_csv3 <- read_csv(paste0(path1, "/telpalen/telpalen.csv"))

test_csv[, c("device_name", "year_cnt")]
test_csv3[, c("device_name", "year_cnt")]

# check data and sort on year count of station
test_csv3 %>%
  glimpse() %>%
  summary()

test_csv3_count <- test_csv3 %>%
  select(id, device_name, road_nl, descr_nl, hour_cnt, day_cnt, year_cnt) %>%
  arrange(desc(year_cnt))

test_csv3_count

#to write away file
#write_csv(test_csv3, paste0(path1, "/telpalen/telpalen.csv"))



