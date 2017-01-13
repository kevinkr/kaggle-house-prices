# House prices Kaggle competition
# Start: 12-26-16
# SVM
# All factors must be numeric
library(caret)

train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice

X_train <- data.matrix(X_train)
X_test <- data.matrix(X_test)
# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE
                                 )

set.seed(99)

grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
                    )

svm.radial.caret <- train(x=X_train, y=y,
                  method="svmRadial",
                  trControl=CARET.TRAIN.CTRL,
                  tuneGrid = grid,
                  maximize=FALSE,
                  metric="RMSE")  

print(svm.radial.caret)

mean(svm.radial.caret$resample$RMSE)

resultsTableExport <- cbind(resultsTable,Model="svmRadial",lowestRmse=mean(svm.radial.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)


svm.linear.caret <- train(x=X_train, y=y,
                   method="svmLinear",
                   trControl=CARET.TRAIN.CTRL,
                   maximize=FALSE,
                   metric="RMSE")  

print(svm.linear.caret)

mean(svm.linear.caret$resample$RMSE)

resultsTableExport <- cbind(resultsTable,Model="svmlinear",lowestRmse=mean(svm.linear.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

grid <- expand.grid(Loss = c("L1", "L2"),
                    cost = c(0.001, 0.01, 0.05, 0.1, 0.25, 1, 2, 4, 6, 8, 10, 15, 20, 30, 50, 75, 100))

CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE
                                 #preProc = c("center", "scale")
                                  )

svm.linear3.caret <- train(x=X_train, y=y,
                          method="svmLinear3",
                          trControl=CARET.TRAIN.CTRL,
                          maximize=FALSE,
                          tuneGrid = grid,
                          svr_eps = c(0.01, 0.25, 0.5, 1, 2, 3),
                          metric="RMSE")  

print(svm.linear3.caret)

mean(svm.linear3.caret$resample$RMSE)

resultsTableExport <- cbind(resultsTable,Model="svmlinear3",lowestRmse=mean(svm.linear3.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(svm.linear.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-svmlinear-v2-1-25-17.csv", row.names = FALSE)

# PL 0.24126  CV 0.2099