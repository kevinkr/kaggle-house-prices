# House prices Kaggle competition
# Start: 12-26-16
# Extreme Learning Machine
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

#enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
#                        .fraction = seq(.05, 1, length = 20))
tuneGrid = expand.grid(pruned = c("No","Yes"), 
                       smoothed = c("Yes","No"), 
                       rules = c("No","Yes"))
# tuneGrid = expand.grid(pruned = "Yes", 
#                        smoothed = c("Yes"), 
#                        rules = c("No"))
# if errors, see http://stackoverflow.com/questions/41878226/using-rweka-m5p-in-rstudio-yields-java-lang-noclassdeffounderror-no-uib-cipr-ma

m5.caret <- train(x=X_train, y=y,
                    method="M5",
                    trControl=CARET.TRAIN.CTRL, 
                    maximize=FALSE,
                    tuneGrid = tuneGrid,
                    preProc = c("center", "scale"),
                    metric="RMSE") 

print(m5.caret)
plot(m5.caret)
mean(m5.caret$resample$RMSE)

print(varImp(m5.caret, scale = FALSE))
plot(varImp(m5.caret, scale = FALSE), main="Variable Importance using M5 Regression")

resultsTableExport <- cbind(resultsTable,Model="M5",lowestRmse=mean(m5.caret$resample$RMSE))
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)

# make create submission file
preds <- exp(predict(m5.caret,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-m5-v1-2-8-17.csv", row.names = FALSE)
