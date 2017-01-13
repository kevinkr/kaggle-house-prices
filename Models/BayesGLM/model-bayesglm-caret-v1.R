# House prices Kaggle competition
# Start: 12-26-16
# Bayesian GLM
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
                                 #preProc = c("center", "scale"),
                                 verboseIter=FALSE)

set.seed(99)

bayesglm.caret <- train(x=X_train, y=y,
                   method="bayesglm",
                   trControl=CARET.TRAIN.CTRL, 
                   maximize=FALSE,
                   metric="RMSE") 

print(bayesglm.caret)

mean(bayesglm.caret$resample$RMSE)

resultsTableExport <- cbind(resultsTable,Model="bayesglm",lowestRmse=mean(bayesglm.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(bayesglm.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-bayesglm-v2-1-25-17.csv", row.names = FALSE)
