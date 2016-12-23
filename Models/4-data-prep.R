
# Data Modeling Preparation -----------------------------------------------
# load previous code
#source("Code/2-1-eda.R")




##########################################
# split back into test and train
library(dplyr)
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
fullSet <- fullSet %>% mutate_each_(funs(factor), cat.var)
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
#test <- subset(test, select = -c(SalePrice))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))
test$SalePrice <- -99
rm(train.raw, test.raw)
gc()


set.seed(1469)
train <- subset(train, select = -c(Id))
rowsTrain <- createDataPartition(train$SalePrice
                                 , p = 0.8
                                 , list = FALSE)
testTrain <- train[rowsTrain,]
validTrain <- train[-rowsTrain,]
