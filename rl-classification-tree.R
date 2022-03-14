##set working directory
setwd("/Users/Rachel/Downloads")

#download required packages 
library(dplyr)
require(tree)

##read data
training_data <- read.csv("census-income-training.csv")
test_data <- read.csv("census-income-test.csv")

#clean up data
clean_training_data <- training_data %>% mutate(income_morethan_50K = factor(income_morethan_50K, levels = c(0, 1), labels = c('No', 'Yes'))) 
clean_training_data  <- na.omit(clean_training_data ) 
test_data  <- na.omit(test_data) 

#fill a model using decision trees
tree.training = tree(income_morethan_50K~.-Id, data=clean_training_data)
summary(tree.training)

#plot classification tree using training data
plot(tree.training)
text(tree.training, pretty = 0)

##prediction 
t_pred = predict(tree.training, test_data, type="class")
confMat <- table(test_data$income_morethan_50K,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)

test_data$income_morethan_50K <- t_pred
test_data <- test_data %>% mutate(income_morethan_50K = factor(income_morethan_50K, levels = c('No', 'Yes'), labels = c(0,1))) 

output= data.frame(Id = test_data$Id, income_morethan_50K = test_data$income_morethan_50K)
write.csv(output, "predictions.csv", row.names = FALSE, quote = F)
