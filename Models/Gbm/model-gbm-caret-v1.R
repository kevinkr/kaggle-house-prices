# House prices Kaggle competition
# Start: 12-26-16
# GBM
# All factors must be numeric
library(caret)

train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice

# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE)

caretGrid <- expand.grid(interaction.depth=c(5, 7), 
                         n.trees = (0:70)*50,
                         shrinkage=c(0.01, 0.001),
                         n.minobsinnode=c(5, 10, 20))

set.seed(99)

gbm.caret <- train(x=X_train, y=y,
                   distribution="gaussian", 
                   method="gbm",
                   trControl=CARET.TRAIN.CTRL, 
                   verbose=FALSE, 
                   tuneGrid=caretGrid, 
                   maximize=FALSE,
                   metric="RMSE", 
                   bag.fraction=0.5)  

print(gbm.caret)
plot(gbm.caret)
varImp(gbm.caret)

mean(gbm.caret$resample$RMSE)

resultsTableExport <- cbind(resultsTable,Model="gbm",lowestRmse=mean(gbm.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(gbm.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-gbm-v2-1-20-17.csv", row.names = FALSE)
