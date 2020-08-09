library(tidyverse)
library(COVID19)
state_data <- covid19(country = 'United states', level = 3) %>% 
  rename(State = administrative_area_level_2) %>% 
  group_by(State, date) %>% 
  summarise(deaths = sum(deaths, na.rm  = TRUE),
            confirmed = sum(confirmed, na.rm = TRUE),
            tests = sum(tests, na.rm = TRUE),
            ) %>% 
  mutate(daily_deaths = deaths - lag(deaths),
         daily_cases = confirmed - lag(confirmed),
         daily_tests = tests - lag(tests),
         daily_positive_rate = daily_cases / daily_tests,
         #seven day rolling totals
         rolling_weekly_cases = roll_sum(daily_cases, 7, fill = NA, align = "right"),
         rolling_weekly_tests = roll_sum(daily_tests, fill = NA, align = "right"),
         rolling_weekly_positive_rate = rolling_weekly_cases / rolling_weekly_tests,
         rolling_weekly_deaths = roll_sum(daily_deaths, 7, fill = NA, align = 'right'),
  ) %>% 
  #keep only dates on or after first confirmed positive test and exclude most recent
  #two days, since data is subject to being updated
  filter(between(date, as.Date('2020-01-22'), Sys.Date() - days(2))) %>% 
  #ungroup because id field no longer needed
  ungroup() %>% 
  #columns to keep
  select(
    State, date, tests, confirmed, deaths, daily_deaths, daily_cases, 
    daily_tests, daily_positive_rate, rolling_weekly_cases,
    rolling_weekly_tests, rolling_weekly_positive_rate, rolling_weekly_deaths
  ) %>% 
  arrange(State, date)


data <- covid19(country = 'United states') %>% 
  mutate(daily_deaths = deaths - lag(deaths),
         daily_cases = confirmed - lag(confirmed),
         daily_tests = tests - lag(tests),
         daily_positive_rate = daily_cases / daily_tests,
         #seven day rolling totals
         rolling_weekly_cases = roll_sum(daily_cases, 7, fill = NA, align = "right"),
         rolling_weekly_tests = roll_sum(daily_tests, fill = NA, align = "right"),
         rolling_weekly_positive_rate = rolling_weekly_cases / rolling_weekly_tests,
         rolling_weekly_deaths = roll_sum(daily_deaths, 7, fill = NA, align = 'right'),
  ) %>% 
  #keep only dates on or after first confirmed positive test and exclude most recent
  #two days, since data is subject to being updated
  filter(between(date, as.Date('2020-01-22'), Sys.Date() - days(2))) %>% 
  #ungroup because id field no longer needed
  ungroup() %>% 
  #columns to keep
  select(
    date, tests, confirmed, deaths, daily_deaths, daily_cases, 
    daily_tests, daily_positive_rate, rolling_weekly_cases,
    rolling_weekly_tests, rolling_weekly_positive_rate, rolling_weekly_deaths
  ) 

state_data_long <- state_data %>% pivot_longer(cols = -one_of('State', 'date'),
                                               names_to = 'type',
                                               values_to = 'value') %>% 
  filter(is.finite(value)) #review missing values and infinite values (value divided by 0)

#pivot data to long format, which is easier to use for plotting multiple variables at once:
# column names: date, type (deaths, confirmed cases, etc), value
data_long <- data %>% pivot_longer(cols = -one_of('date'),
                                   names_to = 'type',
                                   values_to = 'value') %>% 
  filter(is.finite(value)) #review missing values and infinite values (value divided by 0)

fst::write.fst(data, 'data/data.fst', compress = 100)
fst::write.fst(data_long, 'data/data_long.fst', compress = 100)
fst::write.fst(state_data, 'data/state_data.fst', compress = 100)
fst::write.fst(state_data_long, 'data/state_data_long.fst', compress = 100)
