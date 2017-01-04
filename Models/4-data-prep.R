
# Data Modeling Preparation -----------------------------------------------
# load previous code

##########################################
# split back into test and train
library(dplyr)
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
fullSet <- fullSet %>% mutate_each_(funs(factor), cat.var)

# OHE for glmnet
# use caret dummyVars function for hot one encoding for categorical features
library(dummies)
fullSet <- dummy.data.frame(fullSet, names=cat.var, sep="_")
colSums(sapply(fullSet, is.na)) > 0 # list of vars with missing values

# create data for training and test
X_train <- fullSet[1:nrow(train),]
X_test <- fullSet[(nrow(train)+1):nrow(fullSet),]
#y <- train$SalePrice
#test$SalePrice <- -99
rm(train.raw, test.raw)
gc()

library(caret)
set.seed(1469)
TestX_train <- cbind(X_train,SalePrice=train$SalePrice)
rowsTrain <- createDataPartition(TestX_train$SalePrice
                                 , p = 0.65
                                 , list = FALSE)
testTrain <- TestX_train[rowsTrain,]
testY <- log(testTrain$SalePrice + 200)
testTrain <- subset(testTrain, select = -c(SalePrice))

validTrain <- TestX_train[-rowsTrain,]
validY <- log(validTrain$SalePrice + 200)
validTrain <- subset(validTrain, select = -c(SalePrice))
