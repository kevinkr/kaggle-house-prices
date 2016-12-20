
# Data Modeling Preparation -----------------------------------------------
# load previous code
#source("Code/2-1-eda.R")

##########################################
# split back into test and train
test <- fullSet[fullSet$isTest==1,]
train <- fullSet[fullSet$isTest==0,]
# drop loss from test set
test <- subset(test, select = -c(SalePrice))
test <- subset(test, select = -c(isTest))
train <- subset(train, select = -c(isTest))

rm(train.raw, test.raw)
gc()


set.seed(1469)
train <- subset(train, select = -c(Id))
rowsTrain <- createDataPartition(train$SalePrice
                                 , p = 0.8
                                 , list = FALSE)
testTrain <- train[rowsTrain,]
validTrain <- train[-rowsTrain,]