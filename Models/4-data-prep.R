
# Data Modeling Preparation -----------------------------------------------

##########################################
# split back into test and train
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
fullSet <- fullSet %>% mutate_each_(funs(factor), cat.var)
# create data for training and test
X_train <- fullSet[1:nrow(train),]
X_test <- fullSet[(nrow(train)+1):nrow(fullSet),]
#y <- train$SalePrice
#test$SalePrice <- -99
rm(train.raw, test.raw)
gc()
