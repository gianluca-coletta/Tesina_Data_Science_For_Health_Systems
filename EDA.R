# Libraries

library(tidyverse)
library(ggplot2)
library(magrittr)
library(dplyr)
library(lubridate)

# Import dataset of USA Influence report from 2017 to 2021

dataset <-  read.csv(file = 'FluNetInteractiveReport_USA.csv')
head(dataset)

# Remove useless columns

dataset <- dataset[, -c(1,2,3,6,8,10,13,22)]
head(dataset)
attach(dataset)
summary(dataset)

# Rename 

# Summarize and group by month

dataset %>% 
  group_by(month = lubridate::floor_date(EDATE, 'month')) %>%
  summarize(SPEC_PROCESSED_NB = sum(SPEC_PROCESSED_NB),
            AH1N12009 = sum(AH1N12009),
            AH3 = sum(AH3),
            ANOTSUBTYPED = sum(ANOTSUBTYPED),
            INF_A = sum(INF_A),
            BYAMAGATA = sum(BYAMAGATA),
            BVICTORIA = sum(BVICTORIA),
            BNOTDETERMINED = sum(BNOTDETERMINED),
            INF_B = sum(INF_B),
            ALL_INF = sum(ALL_INF),
            ALL_INF2 = sum(ALL_INF2))
