# Import dataset 

dataset <-  read.csv(file = 'influnce_Italy_and_SouthAfrica.csv')
head(dataset)

dataset <- dataset[, -c(1)]
head(influence)
summary(dataset)



# Delete row with all NA values

dataset <- subset(dataset, !(is.na(dataset$SPEC_RECEIVED_NB))  & !(is.na(dataset$SPEC_PROCESSED_NB)))
summary(dataset)
