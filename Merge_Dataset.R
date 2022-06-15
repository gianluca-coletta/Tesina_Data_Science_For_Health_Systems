# Dataset of Italy Influence report from 2009 to 2018

influenceItaly <- read.csv(file = 'FluNetInteractiveReport_Italy.csv')
head(influenceItaly)

# Dataset of South Africa Influence report from 2009 to 2018

influenceSouthAfrica <- read.csv(file = 'FluNetInteractiveReport_SouthAfrica.csv')
head(influenceSouthAfrica)

# Merge dataset

influence <- rbind(influenceItaly,influenceSouthAfrica)
head(influence)
attach(influence)

# Remove useless columns

influence <- influence[, -c(2,3,5)]
head(influence)
summary(influence)

write.csv(influence, "C:\\Develop\\Tesina_Data_Science_For_Health_Systems\\influnce_Italy_and_SouthAfrica.csv", row.names=TRUE)
