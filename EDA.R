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

dataset$month[dataset$month == "Jan"] <- "01-Jan"
dataset$month[dataset$month == "Feb"] <- "02-Feb"
dataset$month[dataset$month == "Mar"] <- "03-Mar"
dataset$month[dataset$month == "Apr"] <- "04-Apr"
dataset$month[dataset$month == "May"] <- "05-May"
dataset$month[dataset$month == "Jun"] <- "06-Jun"
dataset$month[dataset$month == "Jul"] <- "07-Jul"
dataset$month[dataset$month == "Aug"] <- "08-Aug"
dataset$month[dataset$month == "Sep"] <- "09-Sep"
dataset$month[dataset$month == "Oct"] <- "10-Oct"
dataset$month[dataset$month == "Nov"] <- "11-Nov"
dataset$month[dataset$month == "Dec"] <- "12-Dec"

# Plot of total positive

dataset %>%
  ggplot(aes(date,total_positive))+
  geom_line(colour = "red", size = 0.75)+
  scale_x_date(breaks = seq(min(dataset$date), max(dataset$date), by = "2 months"), date_labels = "%b-%Y") +
  theme_bw()+
  labs(title = "Positive cases", x = "Date", y = "Number of positive")+
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(0,100000,10000),labels=scales::comma)

# Plot of total processed samples

dataset %>%
  ggplot(aes(date,processed_samples))+
  geom_line(colour = "red", size = 0.75)+
  scale_x_date(breaks= seq(min(dataset$date), max(dataset$date), by = "2 months"), date_labels = "%b-%Y") +
  theme_bw()+
  labs(title = "Processed samples",x = "Date", y = "Number of processed samples")+
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(0,1000000,100000),labels = scales::comma)

# Histogram of number of positives and processed samples 

options(scipen=999)

hist(dataset$total_positive,
     main = "Histogram", 
     xlab ="Positive cases",
     breaks = 30)


hist(dataset$processed_samples,
     main = "Histogram", 
     xlab ="Processed samples",
     breaks = 30)


###### TEST ######

### Normality test ###

# Total positive 

qqnorm(dataset$total_positive)
qqline(dataset$total_positive)

shapiro.test(dataset$total_positive)


boxcox(lm(dataset$total_positive~1), lambda = seq(-10,10,0.11)) 
title("Total positive")

shapiro.test(log(dataset$total_positive))

qqnorm(log(dataset$total_positive))
qqline(log(dataset$total_positive))

# Processed samples 

qqnorm(dataset$processed_samples)
qqline(dataset$processed_samples)

shapiro.test(dataset$processed_samples)


boxcox(lm(dataset$processed_samples~1), lambda = seq(-10,10,0.11)) 
title("Processed samples")

shapiro.test(log(dataset$processed_samples))

qqnorm(log(dataset$processed_samples))
qqline(log(dataset$processed_samples))



# Summary statistics

summary(dataset)

# Filtering based on year pre-covid and post-covid

dataset_pre <- dataset[dataset$year == "2017" | dataset$year == "2018" | dataset$year == "2019", ]
dataset_post <- dataset[dataset$year == "2020" | dataset$year == "2021" | dataset$year == "2022", ]

# Plot rate of total positive cases pre-covid

dataset_pre %>%
  ggplot(aes(date, total_positive / processed_samples *100, group ="1")) +
  geom_line(colour = "red", size = 1) +
  scale_x_date(breaks= seq(min(dataset_pre$date), max(dataset_pre$date), by = "2 months"), date_labels = "%b-%Y") +
  labs(title = "Tasso positività pre-covid", x = "Date", y = "Positivity rate") +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(0,40,5))



dataset %>% 
ggplot(aes(x = year, y = total_positive / processed_samples *100)) +
  geom_point(size = 3, alpha = 0.5, colour = "red") +
  facet_wrap(~ month) +
  labs(title = "Tasso positività per mese pre covid", y = "Positivity rate") +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

# Plot rate of positive cases post-covid

dataset_post %>%
  ggplot(aes(date, total_positive / processed_samples *100, group ="1")) +
  geom_line(colour = "red", size = 1) +
  scale_x_date(breaks= seq(min(dataset_post$date), max(dataset_post$date), by = "2 months"), date_labels = "%b-%Y") +
  labs(title = "Tasso positività post-covid", x = "Date", y = "Positivity rate") +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(0,30,5))


# Plot rate of positive cases for month

dataset %>% 
  ggplot(aes(x = year, y = total_positive / processed_samples *100)) +
  geom_point(size = 3, alpha = 0.5, colour = "red") +
  facet_wrap(~ month) +
  labs(title = "Tasso positività per ogni mese", y = "Positivity rate") +
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))

# Plot Influence A vs Influence B

dataset_types <- dataset[,-c(2,3,4,5,6,7,9,10,11,13,14)]

dataset_types <- dataset_types %>%
  rename(Influence_A = total_A,
         Influence_B = total_B)

dataset_types <- melt(dataset_types, id.vars = 'date', variable.name = 'Types')

plot_type <- dataset_types%>%
  ggplot(aes(date, value))+
  geom_line(aes(colour = Types), size = 0.75)+
  scale_x_date(breaks= seq(min(dataset_types$date), max(dataset_types$date), by = "3 months"), date_labels = "%b-%Y") +
  labs(title = "Casi Influenza A vs Influenza B", x = "Date", y = "Cases")+
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(0,80000,5000),labels=scales::comma)

plot_type

# Plot of all know type of influence pre-covid and post-covid

dataset_pre_types <- dataset_pre[,-c(2,3,4,7,8,11,12,13,14)]

dataset_pre_types <- dataset_pre_types %>%
  rename(AH1N1_2009 = ah1n12009,
         AH3 = ah3,
         BYamagata = byamagata,
         BVictoria = bvictoria)

dataset_pre_types <- melt(dataset_pre_types, id.vars = 'date', variable.name = 'Type')

plot_type_pre <- dataset_pre_types%>%
  ggplot(aes(date, value))+
  geom_line(aes(colour = Type), size = 0.75)+
  scale_x_date(breaks= seq(min(dataset_pre_types$date), max(dataset_pre_types$date), by = "2 months"), date_labels = "%b-%Y") +
  labs(title = "Casi differenti tipologie di influenza pre-covid", x = "Date", y = "Cases" )+ 
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(0,13000,1000),labels=scales::comma)


plot_type_pre


dataset_post_types <- dataset_post[,-c(2,3,4,7,8,11,12,13,14)]

dataset_post_types <- dataset_post_types %>%
  rename(AH1N1_2009 = ah1n12009,
         AH3 = ah3,
         BYamagata = byamagata,
         BVictoria = bvictoria)

dataset_post_types <- melt(dataset_post_types, id.vars = 'date', variable.name = 'Type')

plot_type_post <- dataset_post_types%>%
  ggplot(aes(date, value))+
  geom_line(aes(colour = Type), size = 0.75)+
  scale_x_date(breaks= seq(min(dataset_post_types$date), max(dataset_post_types$date), by = "2 months"), date_labels = "%b-%Y") +
  labs(title = "Casi differenti tipologie di influenza post-covid", x = "Date", y = "Cases" )+
  theme(axis.text.x = element_text(angle = 70, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = seq(0,10000,1000),labels=scales::comma)

plot_type_post



###### ULTERIORI TEST NON USATI ######

### Normality test ###

# Total positive pre covid 

qqnorm(dataset_pre$total_positive)
qqline(dataset_pre$total_positive)

ks.test(dataset_pre$total_positive,pnorm,mean(dataset_pre$total_positive),sd(dataset_pre$total_positive))
shapiro.test(dataset_pre$total_positive)


boxcox(lm(dataset_pre$total_positive~1), lambda = seq(-10,10,0.1)) 
title("Total positive pre covid")

shapiro.test(log(dataset_pre$total_positive))

qqnorm(log(dataset_pre$total_positive))
qqline(log(dataset_pre$total_positive))

# Processed samples pre covid

qqnorm(dataset_pre$processed_samples)
qqline(dataset_pre$processed_samples)

ks.test(dataset_pre$processed_samples,pnorm,mean(dataset_pre$processed_samples),sd(dataset_pre$processed_samples))
shapiro.test(dataset_pre$processed_samples)

boxcox(lm(dataset_pre$processed_samples~1), lambda = seq(-10,10,0.1)) 
title("Processed samples pre covid")

shapiro.test(log(dataset_pre$processed_samples))

qqnorm(log(dataset_pre$processed_samples))
qqline(log(dataset_pre$processed_samples))

################################################################################################################