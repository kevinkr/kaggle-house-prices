# House prices Kaggle competition
# Start: 12-26-16
# Neural Net
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

nnetGrid <- expand.grid(.decay = seq(0, 0.1, .01),
                        .size = c(3:10),
                        
                        ## The next option is to use bagging
                        ## instead of different random
                        ## seeds.
                        .bag = FALSE)

nnet.caret <- train(x=X_train, y=y,
                    method="avNNet",
                    repeats = 10,
                    trControl=CARET.TRAIN.CTRL, 
                    maximize=FALSE,
                    tuneGrid = nnetGrid,
                    preProc = c("center", "scale"),
                    linout = TRUE,
                    trace = FALSE,
                    MaxNWts = 10 * (ncol(X_train) + 1) + 10 + 1,
                    maxit = 500,
                    metric="RMSE") 

print(nnet.caret)
plot(nnet.caret)
mean(nnet.caret$resample$RMSE)

print(varImp(nnet.caret, scale = FALSE))
plot(varImp(nnet.caret, scale = FALSE), main="Variable Importance using elasticnet Regression")

resultsTableExport <- cbind(resultsTable,Model="neuralnet",lowestRmse=mean(nnet.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(nnet.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-neuralnet-v1-1-31-17.csv", row.names = FALSE)
