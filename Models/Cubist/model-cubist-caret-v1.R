# House prices Kaggle competition
# Start: 12-26-16
# Cubist
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

cubistGrid <- expand.grid(committees = 100, neighbors = 9)

cubist.caret <- train(x=X_train, y=y,
                    method="cubist",
                    trControl=CARET.TRAIN.CTRL, 
                    maximize=FALSE,
                    tuneGrid = cubistGrid,
                    #preProc = c("center", "scale"),
                    metric="RMSE") 

print(cubist.caret)
summary(cubist.caret)
mean(cubist.caret$resample$RMSE)

print(varImp(cubist.caret, scale = FALSE))
plot(varImp(cubist.caret, scale = FALSE), main="Variable Importance using elasticnet Regression")

resultsTableExport <- cbind(resultsTable,Model="cubist",lowestRmse=mean(cubist.caret$resample$RMSE))
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
write.csv(submission, "Submissions/caret-cubist-v2-2-7-17.csv", row.names = FALSE)
