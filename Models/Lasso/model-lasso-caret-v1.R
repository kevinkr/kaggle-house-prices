# House prices Kaggle competition
# Start: 12-26-16
# Lasso
# All factors must be numeric
library(caret)

train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice

X_train <- data.matrix(X_train)
X_test <- data.matrix(X_test)
# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=5,
                                 verboseIter=FALSE)

set.seed(99)

lassoGrid <- expand.grid(.fraction = seq(.05, 1, length = 20))

lasso.caret <- train(x=X_train, y=y,
                     method="lasso",
                     trControl=CARET.TRAIN.CTRL, 
                     maximize=FALSE,
                     tuneGrid = lassoGrid,
                     preProc = c("center", "scale"),
                     metric="RMSE") 

print(lasso.caret)
plot(lasso.caret)
mean(lasso.caret$resample$RMSE)

print(varImp(lasso.caret, scale = FALSE))
plot(varImp(lasso.caret, scale = FALSE), main="Variable Importance using Lasso Regression")

resultsTableExport <- cbind(resultsTable,Model="lasso",lowestRmse=mean(lasso.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(ridge.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-lasso-v1-1-31-17.csv", row.names = FALSE)
