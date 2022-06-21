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
         total_positive = all_inf,
         total_negative = all_inf2)

# Convert column date from character to date

dataset$date <- ymd(dataset$date)

# Summarize and group by month

dataset <- dataset %>%
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
            total_positive = sum(total_positive),
            total_negative = sum(total_negative))

# Add column year and month

dataset$year <- strftime(dataset$date, "%Y")
dataset$month <- strftime(dataset$date, "%m")

# Reorder variable

dataset <- dataset[, c(1,14,13,2,3,4,5,6,7,8,9,10,11,12)]

# Convert month number to month name

dataset$month <- month.abb[as.numeric(dataset$month)]


# Histogram of the ratio between number of positives and samples processed 

par(mfrow=c(1,2))
hist(dataset$total_positive/dataset$processed_samples,
     main = "Histogram", 
     xlab ="Ratio between number of positive and samples processed",
     breaks = 20)


# Histogram of the ratio between number of negative and samples processed 

hist(dataset$total_negative/dataset$processed_samples,
          main = "Histogram", 
          xlab = "Ratio between number of negatives and samples processed",
          breaks = 20)


# Summary statistics

summary(dataset)

# Filtering based on year pre-covid and post-covid

dataset_pre <- dataset[dataset$year == "2017" | dataset$year == "2018" | dataset$year == "2019", ]
dataset_post <- dataset[dataset$year == "2020" | dataset$year == "2021", ]

# Plot total positive cases pre-covid

total_positive_pre <- ggplot(dataset_pre, aes(total_positive_pre)) 
total_positive_pre 









