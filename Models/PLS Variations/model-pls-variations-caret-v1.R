# House prices Kaggle competition
# Start: 12-26-16
# PLS Variations
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

pls.caret <- train(x=X_train, y=y,
                   method="pls",
                   trControl=CARET.TRAIN.CTRL, 
                   maximize=FALSE,
                   tuneLength = 20,
                   preProc = c("center", "scale"),
                   metric="RMSE") 

print(pls.caret)
plot(pls.caret)
mean(pls.caret$resample$RMSE)

print(varImp(pls.caret, scale = FALSE))
plot(varImp(pls.caret, scale = FALSE), main="Variable Importance using PLS Regression")

resultsTableExport <- cbind(resultsTable,Model="pls",lowestRmse=mean(pls.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S")
csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="")
write.csv(resultsTableExport, file=csvFileName)
rm(resultsTableExport)
####################
#  Wide Kernel PLS
widekernelpls.caret <- train(X_train, y=y,
                   method="widekernelpls",
                   trControl=CARET.TRAIN.CTRL, 
                   maximize=FALSE,
                   tuneLength = 20,
                   preProc = c("center", "scale"),
                   metric="RMSE") 

print(widekernelpls.caret)
plot(widekernelpls.caret)
mean(widekernelpls.caret$resample$RMSE)

print(varImp(widekernelpls.caret, scale = FALSE))
plot(varImp(widekernelpls.caret, scale = FALSE), main="Variable Importance using Wide Kernel PLS Regression")

####################
#  Kernel PLS
kernelpls.caret <- train(X_train, y=y,
                             method="kernelpls",
                             trControl=CARET.TRAIN.CTRL, 
                             maximize=FALSE,
                             tuneLength = 20,
                             preProc = c("center", "scale"),
                             metric="RMSE") 

print(kernelpls.caret)
plot(kernelpls.caret)
mean(kernelpls.caret$resample$RMSE)

print(varImp(kernelpls.caret, scale = FALSE))
plot(varImp(kernelpls.caret, scale = FALSE), main="Variable Importance using Kernel PLS Regression")






# make create submission file
preds <- exp(predict(enet.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-elasticnet-v2-2-7-17.csv", row.names = FALSE)
