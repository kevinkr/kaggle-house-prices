# Kaggle home prices
# Caret Ensemble
library("caret")
library("mlbench")
library("pROC")
library("caretEnsemble")

TestX_train <- cbind(X_train,SalePrice=train$SalePrice)
rowsTrain <- createDataPartition(TestX_train$SalePrice
                                 , p = 0.8
                                 , list = FALSE)
testTrain <- TestX_train[rowsTrain,]
testY <- log(testTrain$SalePrice + 200)
testTrain <- subset(testTrain, select = -c(SalePrice))
testTrain <- data.matrix(testTrain)

validTrain <- TestX_train[-rowsTrain,]
validY <- log(validTrain$SalePrice + 200)
validTrain <- subset(validTrain, select = -c(SalePrice))
validTrain <- data.matrix(validTrain)

# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="boot",
                                 number=25,
                                 repeats=5,
                                 savePredictions = "final",
                                 classProbs=FALSE,
                                 index=createResample(testY, 25),
                                 verboseIter=FALSE)
set.seed(123)  # for reproducibility

glmBoostGrid = expand.grid(mstop = seq(600, 3000, 200),
                           prune = c('yes', 'no'))

model_list <- caretList(x=testTrain, y=testY,
                        trControl=CARET.TRAIN.CTRL,
                        preProc = c("center", "scale"),
                        maximize=FALSE,
                        metric="RMSE",
                        #methodList=c("pls", "glmboost")
                        tuneList=list(
                          glmboost=caretModelSpec(method="glmboost", tuneGrid=glmBoostGrid),
                          pls=caretModelSpec(method="pls", tuneLength = 20)
                        )
)

p <- as.data.frame(predict(model_list, newdata=head(validTrain)))
print(p)

xyplot(resamples(model_list))

# get model correlations
modelCor(resamples(model_list))


greedy_ensemble <- caretEnsemble(model_list, 
                                 metric="RMSE",
                                 trControl=trainControl(
                                   number=25,
                                   classProbs=FALSE
                                 )  
                                )
summary(greedy_ensemble)
varImp(greedy_ensemble)

library("caTools")
model_preds <- lapply(model_list, predict, newdata=validTrain)
model_preds <- exp(data.frame(model_preds)) - 200

ens_preds <- exp(predict(greedy_ensemble, newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = ens_preds
write.csv(submission, "Submissions/caretEnsemble-pls-glmboost-v1-2-2-17.csv", row.names = FALSE)

# PL 0.11929 (best 2-2-17) CV 0.1154