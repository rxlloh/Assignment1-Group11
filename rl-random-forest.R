##set working directory
setwd("/Users/Rachel/Downloads")

#download required packages 
library(dplyr)
require(randomForest)

##read data
training_data2 <- read.csv("census-income-training.csv")
test_data2 <- read.csv("census-income-test.csv")

#check which are the data with ?? and change them to NA
training_data2[ training_data2 == "?"] <- NA
colSums(is.na(training_data2))

#Allocate a different level in variable for categorical data
training_data2$GRINREG[is.na(training_data2$GRINREG)]<- 'Not answered'
training_data2$MIGMTR1[is.na(training_data2$MIGMTR1)]<- 'Not answered'
training_data2$MIGMTR3[is.na(training_data2$MIGMTR3)]<- 'Not answered'
training_data2$MIGSAME[is.na(training_data2$MIGSAME)]<- 'Not answered'
training_data2$PEFNTVTY[is.na(training_data2$PEFNTVTY)]<- 'Not answered'
training_data2$PEMNTVTY[is.na(training_data2$PEMNTVTY )]<- 'Not answered'
training_data2$PENATVTY[is.na(training_data2$PENATVTY )]<- 'Not answered'

##make chr variables factor 
training_data2[sapply(training_data2, is.character)] <- lapply(training_data2[sapply(training_data2, is.character)], 
                                                               as.factor)

training_data2$income_morethan_50K <- as.factor(training_data2$income_morethan_50K)

#create model of random forest 
training_forest = randomForest(income_morethan_50K~., data = training_data2)


#check which are the data with ?? and change them to NA in TEST DATA
test_data2[ test_data2 == "?"] <- NA
colSums(is.na(test_data2))

#Allocate a different level in variable for categorical data
test_data2$GRINREG[is.na(test_data2$GRINREG)]<- 'Not answered'
test_data2$MIGMTR1[is.na(test_data2$MIGMTR1)]<- 'Not answered'
test_data2$MIGMTR3[is.na(test_data2$MIGMTR3)]<- 'Not answered'
test_data2$MIGSAME[is.na(test_data2$MIGSAME)]<- 'Not answered'
test_data2$PEFNTVTY[is.na(test_data2$PEFNTVTY)]<- 'Not answered'
test_data2$PEMNTVTY[is.na(test_data2$PEMNTVTY )]<- 'Not answered'
test_data2$PENATVTY[is.na(test_data2$PENATVTY )]<- 'Not answered'
output= data.frame(Id = test_data2$Id)

##make chr variables factor 
test_data2[sapply(test_data2, is.character)] <- lapply(test_data2[sapply(test_data2, is.character)], 
                                                       as.factor)

#prediction using test data
pred = predict(training_forest, newdata=test_data2)

##create output CSV
output2.1= data.frame(Id = output$Id, income_morethan_50K = pred)
write.csv(output2.1, "forest_predictions2.csv", row.names = FALSE, quote = F)


