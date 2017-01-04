# House prices Kaggle competition
# Start: 12-26-16
library(caret)


# identify model
resultsTable$model <- 'ctree'

# test <- subset(test, select = -c(SalePrice))
# test.m <- data.matrix(test)
# 
# subTrain.m <- data.matrix(train)
train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice
#train <- subset(train, select = -c(SalePrice))

# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE)

Grid <- expand.grid(mtry = seq(100, 108, 2))

# train model
set.seed(123)  # for reproducibility
model_cforest <- train(x=X_train,
                     y=y,
                     method="cforest",
                     metric="RMSE",
                     maximize=FALSE,
                     tuneGrid=Grid,
                     trControl=CARET.TRAIN.CTRL)

cforestVarImp = varImp(model_cforest)
plot(cforestVarImp, top = 20)


trellis.par.set(caretTheme())
plot(model_cforest)

vGrid <- expand.grid(mtry = seq(104, 108, 2))

set.seed(123)  # for reproducibility
vmodel_cforest <- train(x=testTrain,
                       y=testY,
                       method="cforest",
                       metric="RMSE",
                       maximize=FALSE,
                       tuneGrid=vGrid,
                       trControl=CARET.TRAIN.CTRL)

trellis.par.set(caretTheme())
plot(vmodel_cforest)

validPredict <- predict(vmodel_cforest, validTrain)
postResample(pred = validPredict, obs = validY)

# make create submission file
preds <- exp(predict(model_cforest,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-cforest-v3-1-6-17.csv", row.names = FALSE)
