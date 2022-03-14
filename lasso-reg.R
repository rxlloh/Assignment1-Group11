##set working directory
setwd("/Users/Rachel/Downloads")

#download required packages 
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

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


columns = c('AAGE', 'ACLSWKR', 'ADTIND', 'ADTOCC','AHGA', 'AHRSPAY', 'AHSCOL','AMARITL','AMJIND',
            'AMJOCC', 'ARACE', 'AREORGN', 'ASEX', 'AUNMEM', 'AUNTYPE', 'AWKSTAT', 'CAPGAIN','CAPLOSS', 'DIVVAL', 'FEDTAX',
            'FILESTAT', 'GRINREG', 'GRINST', 'HHDFMX', 'HHDREL', 'MIGMTR1', 'MIGMTR3', 'MIGMTR4', 'MIGSAME',
            'NOEMP', 'PEFNTVTY', 'PEMNTVTY', 'PENATVTY', 'PRCITSHP', 'SEOTR', 'VETQVA', 'VETYN', 'WKSWORK', 'year')

dummyvar <- dummyVars(income_morethan_50K ~ .-Id, data = training_data[,columns])

train_dummies = predict(dummyvar, newdata = training_data[,columns])

test_dummies = predict(dummyvar, newdata = test_data[,columns])

##create test and train data
x = as.matrix(train_dummies)
y_train = training_data$income_morethan_50K

x_test = as.matrix(test_dummies)
y_test = test_data$income_morethan_50K


# building a lasso model to find the optimal lambda value
lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_regression <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# best lamnda value
lambda_best <- lasso_regression$lambda.min 

#train lasso model using optimal lambda value
lasso_model <- glmnet(x, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

#predictions using training data 
predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
eval_results(y_train, predictions_train, train)

#predictions using testing data 
predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)

#create csv
output= data.frame(Id = test_data$Id, income_morethan_50K = predictions_test)
write.csv(output, "lasso_reg.csv", row.names = FALSE, quote = F)

