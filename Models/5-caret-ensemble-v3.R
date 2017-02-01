# Kaggle home prices
# Caret Ensemble
library("caret")
library("mlbench")
library("pROC")
library("caretEnsemble")

#train$SalePrice <- log(train$SalePrice + 200)
#y <- train$SalePrice

#X_train <- data.matrix(X_train)
X_test <- data.matrix(X_test)

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

lassoGrid <- expand.grid(.fraction = seq(.05, 1, length = 20))

caretGrid <- expand.grid(interaction.depth=c(5, 7), 
                         n.trees = (0:70)*50,
                         shrinkage=c(0.01, 0.001),
                         n.minobsinnode=c(5, 10, 20))

model_list <- caretList(x=testTrain, y=testY,
                        trControl=CARET.TRAIN.CTRL,
                        preProc = c("center", "scale"),
                        maximize=FALSE,
                        metric="RMSE",
                        tuneList=list(
                          glmboost=caretModelSpec(method="glmboost", tuneGrid=glmBoostGrid),
                          pls=caretModelSpec(method="pls", tuneLength = 20),
                          lasso=caretModelSpec(method="lasso", tuneGrid=lassoGrid),
                          gbm=caretModelSpec(method="gbm",distribution="gaussian",bag.fraction=0.5)
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
                                   number=50,
                                   classProbs=FALSE
                                 )  
                                )
summary(greedy_ensemble)
varImp(greedy_ensemble)

ens_preds <- exp(predict(greedy_ensemble, newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = ens_preds
write.csv(submission, "Submissions/caretEnsemble-pls-glmboost-lasso-gbm-v2-2-4-17.csv", row.names = FALSE)

# PL 0.12249 CV 0.109