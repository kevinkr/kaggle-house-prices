# House prices Kaggle competition
# Start: 12-26-16
# GLMBoost
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

#tuneGrid = expand.grid(mstop = seq(600, 3000, 200),
                           #prune = c('yes', 'no'))

glmboost.caret <- train(x=X_train, y=y,
                        method="randomGLM",
                        trControl=CARET.TRAIN.CTRL, 
                        maximize=FALSE,
                        #tuneGrid = glmBoostGrid,
                        metric="RMSE") 

print(glmboost.caret)
plot(glmboost.caret)
mean(glmboost.caret$resample$RMSE)

resultsTableExport <- cbind(resultsTable,Model="glmboost",lowestRmse=mean(glmboost.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(glmboost.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-glmboost-v6-1-31-17.csv", row.names = FALSE)
# CV .111673 PL 0.12050
# CV .1124172 PL 0.12032 (best so far 1-30-17)
