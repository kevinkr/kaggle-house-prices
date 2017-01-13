# House prices Kaggle competition
# Start: 12-26-16
# ElasticNet
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

enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
                        .fraction = seq(.05, 1, length = 20))

enet.caret <- train(x=X_train, y=y,
                     method="enet",
                     trControl=CARET.TRAIN.CTRL, 
                     maximize=FALSE,
                     tuneGrid = enetGrid,
                     preProc = c("center", "scale"),
                     metric="RMSE") 

print(enet.caret)
plot(enet.caret)
mean(enet.caret$resample$RMSE)

print(varImp(enet.caret, scale = FALSE))
plot(varImp(enet.caret, scale = FALSE), main="Variable Importance using elasticnet Regression")

resultsTableExport <- cbind(resultsTable,Model="elasticnet",lowestRmse=mean(enet.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(enet.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-elasticnet-v1-1-31-17.csv", row.names = FALSE)
