# House prices Kaggle competition
# Start: 12-26-16
# GAMBoost
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

gamBoostGrid = expand.grid(mstop = seq(100, 200, 100),
                           prune = c('yes', 'no'))


gamboost.caret <- train(x=X_train, y=y,
                        method="gamboost",
                        trControl=CARET.TRAIN.CTRL, 
                        maximize=FALSE,
                        tuneGrid = gamBoostGrid,
                        metric="RMSE") 

print(gamboost.caret)
plot(gamboost.caret)
mean(gamboost.caret$resample$RMSE)

resultsTableExport <- cbind(resultsTable,Model="gamboost",lowestRmse=mean(gamboost.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(gamboost.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-gamboost-v1-1-30-17.csv", row.names = FALSE)

