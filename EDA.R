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

dataset <- dataset[, -c(1,2,3,5,6,8,10,13,22)]
head(dataset)
summary(dataset)

# Rename columns

dataset<- dataset %>%
  rename_with(tolower)

dataset <- dataset %>%
  rename(date = edate,
         processed_samples = spec_processed_nb,
         total_A = inf_a,
         total_B = inf_b,
         totale_positve = all_inf,
         totale_negative = all_inf2)

# Convert column date from character to date

dataset$date <- ymd(dataset$date)

# Summarize and group by month

dataset_new <- dataset %>%
  group_by(date = lubridate::floor_date(date, 'month')) %>%
  summarize(processed_samples = sum(processed_samples),
            ah1n12009 = sum(ah1n12009),
            ah3 = sum(ah3),
            anotsubtyped = sum(anotsubtyped),
            total_A = sum(total_A),
            byamagata = sum(byamagata),
            bvictoria = sum(bvictoria),
            bnotdetermined = sum(bnotdetermined),
            total_B = sum(total_B),
            totale_positve = sum(totale_positve),
            totale_negative = sum(totale_negative))

summary(dataset_new)

# Add column year and month

dataset_new$year <- strftime(dataset_new$date, "%Y")
dataset_new$month <- strftime(dataset_new$date, "%m")

# Reorder variable

dataset_new <- dataset_new[, c(1,14,13,2,3,4,5,6,7,8,9,10,11,12)]

# Convert month number to month name

dataset_new$month <- month.abb[as.numeric(dataset_new$month)]

