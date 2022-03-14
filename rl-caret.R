##set working directory
setwd("/Users/Rachel/Downloads")

#download required packages 
library(dplyr)
library(randomForest)
library(mlbench)
library(caret)

##read data
training_data <- read.csv("census-income-training.csv")
test_data <- read.csv("census-income-test.csv")

x <- training_data[,1:40]
y <- training_data[,41]

#check which are the data with ?? and change them to NA
training_data[ training_data == "?"] <- NA
colSums(is.na(training_data))

#Allocate a different level in variable for categorical data
training_data$GRINREG[is.na(training_data$GRINREG)]<- 'Not answered'
training_data$MIGMTR1[is.na(training_data$MIGMTR1)]<- 'Not answered'
training_data$MIGMTR3[is.na(training_data$MIGMTR3)]<- 'Not answered'
training_data$MIGSAME[is.na(training_data$MIGSAME)]<- 'Not answered'
training_data$PEFNTVTY[is.na(training_data$PEFNTVTY)]<- 'Not answered'
training_data$PEMNTVTY[is.na(training_data$PEMNTVTY )]<- 'Not answered'
training_data$PENATVTY[is.na(training_data$PENATVTY )]<- 'Not answered'
training_data$PARENT <-NULL


training_data[sapply(training_data, is.character)] <- lapply(training_data[sapply(training_data, is.character)], 
                                                             as.factor)

training_data$income_morethan_50K <- as.factor(training_data$income_morethan_50K)


# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=10)
set.seed(12345)
gbmFit1 <- train(income_morethan_50K~.-Id, data = training_data, 
                 method = "gbm", 
                 trControl = control,
                 verbose = FALSE)


#prediction using model 
pred <- predict(gbmFit1, newdata = head(test_data), type = "prob")

#create csv
output= data.frame(Id = test_data$Id, income_morethan_50K = pred)
write.csv(output, "CARET.csv", row.names = FALSE, quote = F)

