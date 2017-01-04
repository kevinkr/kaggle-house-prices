# House prices Kaggle competition
# Start: 12-26-16
library(caret)

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

Grid <- expand.grid(degree = seq(5, 105, 10), nprune)

# train model
set.seed(123)  # for reproducibility
model_cforest <- train(x=X_train,
                       y=y,
                       method="earth",
                       metric="RMSE",
                       maximize=FALSE,
                       tuneGrid=Grid,
                       trControl=CARET.TRAIN.CTRL)

cforestVarImp = varImp(model_cforest)

trellis.par.set(caretTheme())
plot(model_cforest)

# make create submission file
preds <- exp(predict(model_cforest,newdata=X_test)) - 200

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-cforest-v1-1-5-17.csv", row.names = FALSE)

robustControl <- trainControl(summaryFunction = madSummary)
marsGrid <- expand.grid(.degree = 1,
                        .nprune = (1:10) * 2)

earthFit <- train(medv ~ .,
                  data = BostonHousing, 
                  "earth",
                  tuneGrid = marsGrid,
                  metric = "MAD",
                  maximize = FALSE,
                  trControl = robustControl)