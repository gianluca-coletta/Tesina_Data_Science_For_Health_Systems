# Libraries

library(tidyverse)
library(ggplot2)
library(magrittr)
library(dplyr)
library(lubridate)
library(reshape2)
require(gridExtra)
library(MASS)

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

# Rename month

dataset$month[dataset$month == "Jan"] <- "01-January"
dataset$month[dataset$month == "Feb"] <- "02-February"
dataset$month[dataset$month == "Mar"] <- "03-March"
dataset$month[dataset$month == "Apr"] <- "04-April"
dataset$month[dataset$month == "May"] <- "05-May"
dataset$month[dataset$month == "Jun"] <- "06-June"
dataset$month[dataset$month == "Jul"] <- "07-July"
dataset$month[dataset$month == "Aug"] <- "08-August"
dataset$month[dataset$month == "Sep"] <- "09-September"
dataset$month[dataset$month == "Oct"] <- "10-October"
dataset$month[dataset$month == "Nov"] <- "11-November"
dataset$month[dataset$month == "Dec"] <- "12-December"


# Histogram of the ratio between number of positives and samples processed 

par(mfrow=c(1,2))

hist(dataset$total_positive/dataset$processed_samples,
     main = "Istogramma", 
     xlab ="Totale positivi / campioni processati",
     breaks = 20)


# Histogram of the ratio between number of negative and samples processed 

hist(dataset$total_negative/dataset$processed_samples,
          main = "Istogramma", 
          xlab = "Totale negativi / campioni processati",
          breaks = 15)


# Summary statistics

summary(dataset)

# Filtering based on year pre-covid and post-covid

dataset_pre <- dataset[dataset$year == "2017" | dataset$year == "2018" | dataset$year == "2019", ]
dataset_post <- dataset[dataset$year == "2020" | dataset$year == "2021", ]

# Plot total positive cases pre-covid

total_positive_pre_year <- ggplot(data = dataset_pre,
                             mapping= aes(x = month(date),
                                          y = total_positive/processed_samples*100, colour = month))+
                          geom_point(size = 3) + 
                          geom_line(colour = "red")+
                          facet_wrap(~year(date))+
                          labs(title = "Andamento percentuale positivi per anno")+
                          theme_bw()
total_positive_pre_year 

total_positive_pre_month <- ggplot(data = dataset_pre,
                             mapping= aes(x = year,
                                          y = total_positive/processed_samples*100))+
  geom_point(size = 3, alpha = 0.5) + 
  facet_wrap(~month(date))+
  labs(title = "Andamento percentuale positivi per mese")+
  theme_bw()

total_positive_pre_month

# Box-plot for each type of influence pre-covid

# Total A
dataset_pre %>%
  ggplot(aes(year,total_A))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Concentration of influence type-A pre-covid")

# Total B
dataset_pre %>%
  ggplot(aes(year,total_B))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Concentration of influence type-B pre-covid")


# Plot total positive cases post-covid

total_positive_post_year <- ggplot(data = dataset_post,
                                  mapping= aes(x = month(date),
                                               y = total_positive/processed_samples*100, colour = month))+
  geom_point(size = 3) + 
  geom_line(colour = "red")+
  facet_wrap(~year(date))+
  labs(title = "Andamento percentuale positivi per anno")+
  theme_bw()
total_positive_post_year 

total_positive_post_month <- ggplot(data = dataset_post,
                                   mapping= aes(x = year,
                                                y = total_positive/processed_samples*100))+
  geom_point(size = 3, alpha = 0.5) + 
  facet_wrap(~month(date))+
  labs(title = "Andamento percentuale positivi per mese")+
  theme_bw()

total_positive_post_month

# Box-plot for each type of influence post-covid

# Total A
dataset_post %>%
  ggplot(aes(year,total_A))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Concentration of influence type-A post-covid")

# Total B
dataset_post %>%
  ggplot(aes(year,total_B))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Concentration of influence type-B post-covid")

# Total B without outlier 
dataset_post %>%
  filter(total_B<10000) %>%
  ggplot(aes(year,total_B))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Concentration of influence type-B post-covid")


# Total_positive_pre_year and total_positive_post_year

grid.arrange(total_positive_pre_year,total_positive_post_year,ncol=2)

# Plot of all know type of influence for year pre-covid and post-covid

dataset_pre_types <- dataset_pre[,-c(2,3,4,7,8,11,12,13,14)]

dataset_pre_types <- melt(dataset_pre_types, id.vars = 'date', variable.name = 'type')

plot_type_pre <- dataset_pre_types%>%
  ggplot(aes(date, value))+
  geom_point(size = 2)+
  geom_line(aes(colour = type))


dataset_post_types <- dataset_post[,-c(2,3,4,7,8,11,12,13,14)]

dataset_post_types <- melt(dataset_post_types, id.vars = 'date', variable.name = 'type')

plot_type_post <- dataset_post_types%>%
  ggplot(aes(date, value))+
  geom_point(size = 2)+
  geom_line(aes(colour = type))

grid.arrange(plot_type_pre,plot_type_post,ncol=2)



# plot dataset_pre_types group by year

dataset_pre_types_year <- aggregate(value~year(date)+type, data=dataset_pre_types, FUN=sum) 

dataset_post_types_year <- aggregate(value~year(date)+type, data=dataset_post_types, FUN=sum)

colnames(dataset_pre_types_year) <- c('year','type','value')
colnames(dataset_post_types_year) <- c('year','type','value')

plot_type_pre_year <- dataset_pre_types_year%>%
  ggplot(aes(year, value))+
  geom_point(size = 2)+
  geom_line(aes(colour = type))

plot_type_post_year <- dataset_post_types_year%>%
  ggplot(aes(year, value))+
  geom_point(size = 2)+
  geom_line(aes(colour = type))

grid.arrange(plot_type_pre_year,plot_type_post_year,ncol=2)


###### TEST ######

# Total positive pre

qqnorm(dataset_pre$total_positive)
qqline(dataset_pre$total_positive)

ks.test(dataset_pre$total_positive,pnorm,mean(dataset_pre$total_positive),sd(dataset_pre$total_positive))
shapiro.test(dataset$total_positive)


boxcox(lm(dataset_pre$total_positive~1), lambda = seq(-10,10,0.1)) 
title("Total positive")

shapiro.test((dataset_pre$total_positive^6-1)/6)

qqnorm((dataset_pre$total_positive^6-1)/6)
qqline((dataset_pre$total_positive^6-1)/6)

# Processed samples pre

qqnorm(dataset_pre$processed_samples)
qqline(dataset_pre$processed_samples)

ks.test(dataset_pre$processed_samples,pnorm,mean(dataset_pre$processed_samples),sd(dataset_pre$processed_samples))
shapiro.test(dataset_pre$processed_samples)

boxcox(lm(dataset_pre$processed_samples~1), lambda = seq(-10,10,0.1)) 
title("Processed samples")

shapiro.test((dataset_pre$processed_samples^6-1)/6)

qqnorm((dataset_pre$processed_samples^6-1)/6)
qqline((dataset_pre$processed_samples^6-1)/6)

