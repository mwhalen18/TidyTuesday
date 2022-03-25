library(tidyverse)
library(lubridate)
library(tidytuesdayR)

data = tt_load("2022", 10)

cran = data$cran

cran %>% 
  mutate(long_day = as.Date(date, format = "%a %b %d"),
         dday = as_date(date),
         new_date = coalesce(long_day, dday)) %>% 
  filter(is.na(new_date)) %>% 
  View()
