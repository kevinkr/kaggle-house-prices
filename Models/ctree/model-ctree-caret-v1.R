# House prices Kaggle competition
# Start: 12-26-16
library(caret)

# test <- subset(test, select = -c(SalePrice))
# test.m <- data.matrix(test)
# 
# subTrain.m <- data.matrix(train)
train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice
#train <- subset(train, select = -c(SalePrice))

# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE)

Grid <- expand.grid(mincriterion = seq(0, 0.95, 0.05))

# train model
set.seed(123)  # for reproducibility
model_ctree <- train(x=X_train,
                     y=y,
                     method="ctree",
                     metric="RMSE",
                     maximize=FALSE,
                     tuneGrid=Grid,
                     trControl=CARET.TRAIN.CTRL)

ctreeVarImp = varImp(model_ctree)


Grid <- expand.grid(maxdepth = seq(15, 50,5),mincriterion = seq(0, 0.95, 0.05))
set.seed(123)  # for reproducibility
model_ctree2 <- train(x=X_train,
                     y=y,
                     method="ctree2",
                     metric="RMSE",
                     maximize=FALSE,
                     tuneGrid=Grid,
                     trControl=CARET.TRAIN.CTRL)

ctree2VarImp = varImp(model_ctree2)



# make create submission file
preds <- exp(predict(model_ctree2,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-ctree2-v1-1-5-17.csv", row.names = FALSE)
