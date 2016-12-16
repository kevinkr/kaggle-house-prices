
# Data Modeling Preparation -----------------------------------------------
# load previous code
#source("Code/2-1-eda.R")

library("caret")
set.seed(1469)
rowsTrain <- createDataPartition(train$SalePrice
                                 , p = 0.8
                                 , list = FALSE)

testTrain <- train[rowsTrain,]
validTrain <- train[-rowsTrain,]


