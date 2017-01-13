# House prices Kaggle competition
# Start: 12-26-16
# lm
# All factors must be numeric
library(caret)

train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice



# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE,
                                 preProc = c("center"))

set.seed(99)

lm.caret <- train(x=X_train, y=y,
                   method="lm",
                   trControl=CARET.TRAIN.CTRL,
                   maximize=FALSE,
                   metric="RMSE")  

print(lm.caret)

mean(lm.caret$results$RMSE)

resultsTableExport <- cbind(resultsTable,Model="gbm",lowestRmse=mean(lm.caret$results$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(lm.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-lm-v1-1-20-17.csv", row.names = FALSE)


