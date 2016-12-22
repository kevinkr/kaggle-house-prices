
# Data Modeling Preparation -----------------------------------------------
# load previous code
#source("Code/2-1-eda.R")

###########################################
# Feature Engineering - Basement
# Drop some levels
fullSet <- subset(fullSet, select = -c(BsmtHalfBath,BsmtFinSF2,BsmtFinType2,BsmtFullBath, 
                                       BsmtCond, BsmtUnfSF, BsmtExposure))
# Create new features
fullSet$newBsmtQualSF <- as.numeric(factor(fullSet$BsmtQual, levels=c("No Bsmnt", "Po",
                                                                      "Fa", "TA", "Gd", "Ex")))
fullSet$newBsmtQualSF <- fullSet$newBsmtQualSF * fullSet$TotalBsmtSF

# Bsmt Fin Type 1
fullSet$newBsmtFinTypeSF <- as.numeric(factor(fullSet$BsmtFinType1, levels=c("No Bsmnt", "Unf",
                                                                             "LWQ", "Rec", "BLQ", "ALQ", "GLQ")))
fullSet$newBsmtFinTypeSF <- fullSet$newBsmtFinTypeSF * fullSet$BsmtFinSF1


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
