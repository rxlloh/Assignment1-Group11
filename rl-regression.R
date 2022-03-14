##set working directory
setwd("/Users/Rachel/Downloads")

#download required packages
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)

##read data
training_data <- read.csv("census-income-training.csv")
test_data <- read.csv("census-income-test.csv")

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

#check which are the data with ?? and change them to NA in TEST DATA
test_data2[ test_data2 == "?"] <- NA
colSums(is.na(test_data2))

#Allocate a different level in variable for categorical data
test_data$GRINREG[is.na(test_data$GRINREG)]<- 'Not answered'
test_data$MIGMTR1[is.na(test_data$MIGMTR1)]<- 'Not answered'
test_data$MIGMTR3[is.na(test_data$MIGMTR3)]<- 'Not answered'
test_data$MIGSAME[is.na(test_data$MIGSAME)]<- 'Not answered'
test_data$PEFNTVTY[is.na(test_data$PEFNTVTY)]<- 'Not answered'
test_data$PEMNTVTY[is.na(test_data$PEMNTVTY )]<- 'Not answered'
test_data$PENATVTY[is.na(test_data$PENATVTY )]<- 'Not answered'
test_data$PARENT <-NULL

##make chr variables factor 
training_data[sapply(training_data, is.character)] <- lapply(training_data[sapply(training_data2, is.character)], 
                                                               as.factor)


test_data[sapply(test_data, is.character)] <- lapply(test_data[sapply(test_data, is.character)], 
                                                       as.factor)


#scale numeric feature 
print(select_if(training_data, is.numeric))

cols = c('AAGE', 'ADTIND', 'ADTOCC', 'AHRSPAY', 'CAPGAIN', 'CAPLOSS', 'DIVVAL', 
'HHDREL', 'NOEMP', 'SEOTR', 'VETYN', 'WKSWORK', 'year')

preprocess_val <- preProcess(training_data[,cols], method = c("center", "scale"))

training_data[,cols] = predict(preprocess_val, training_data[,cols])
test_data[,cols] = predict(preprocess_val, test_data[,cols])

#build a linear model
linear_regression = lm(income_morethan_50K ~ ., data = training_data)


#create the evaluation metrics function

evaluation = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 2))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 2))
  print(adj_r2) #Adjusted R-squared
  print(as.character(round(sqrt(sum(resids2)/N), 2))) #RMSE
}

#predicting and evaluating the model on train data
predictions = predict(linear_regression, newdata = training_data)
evaluation(linear_regression, training_data, predictions, target = 'income_morethan_50K')

#predicting and evaluating the model on test data
predictions = predict(linear_regression, newdata = test_data)
evaluation(linear_regression, test_data, predictions, target = 'income_morethan_50K')

#create csv
output= data.frame(Id = test_data$Id, income_morethan_50K = pred)
write.csv(output, "regression.csv", row.names = FALSE, quote = F)






