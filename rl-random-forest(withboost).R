##set working directory
setwd("/Users/Rachel/Downloads")

#download required packages 
library(dplyr)
require(randomForest)
require(gbm)

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

#use boosting to find out which variables have high relative importance and which has none
boost.data = gbm(income_morethan_50K~., data = training_data3, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.data)

#remove ID which shouldn't be used for prediction and also four variables that have low relative importance according to boost 
training_data2$Id <- NULL
training_data2$AHSCOL <- NULL
training_data2$FILESTAT <- NULL
training_data2$PARENT <- NULL
training_data2$year <- NULL


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
test_data2$Id <- NULL
test_data2$AHSCOL <- NULL
test_data2$FILESTAT <- NULL
test_data2$PARENT <- NULL
test_data2$year <- NULL

test_data2[sapply(test_data2, is.character)] <- lapply(test_data2[sapply(test_data2, is.character)], 
                                                       as.factor)

#predict 
pred = predict(training_forest, newdata=test_data2)

#create csv
output2.1= data.frame(Id = output$Id, income_morethan_50K = pred)
write.csv(output2.1, "forest_predictions2.csv", row.names = FALSE, quote = F)


