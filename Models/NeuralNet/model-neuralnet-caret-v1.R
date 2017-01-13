# House prices Kaggle competition
# Start: 12-26-16
# Neural Network
# All factors must be numeric
library(caret)

train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice


# X_train <- data.matrix(X_train)
# X_test <- data.matrix(X_test)

# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=5,
                                 #preProc = c("center", "scale"),
                                 verboseIter=FALSE)

tune.grid <- expand.grid(.decay = c(0.1, 0.01, 0.0075), .size = c(1, 10, 20))

set.seed(99)

nn.caret <- train(x=X_train, y=y,
                        method="nnet",
                        trControl=CARET.TRAIN.CTRL, 
                        maximize=FALSE,
                        metric="RMSE",
                        maxit = 1000,
                        tuneGrid = tune.grid,
                        linout = 1,
                        MaxNWts=8000,
                        trace = F) 

print(nn.caret)
plot(nn.caret)
mean(nn.caret$resample$RMSE)

resultsTableExport <- cbind(resultsTable,Model="nn",lowestRmse=mean(nn.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(nn.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-neuralnet-v1-1-22-17.csv", row.names = FALSE)

#1-22-17: CV 0.153, PL 0.134
