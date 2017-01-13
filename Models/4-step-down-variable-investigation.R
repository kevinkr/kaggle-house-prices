# Kaggle home prices
# automating step down variable selection

# set up first mean sample based on current variables
library(caret)
train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice
XX_train <- X_train
X_train <- data.matrix(X_train)
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=5,
                                 verboseIter=FALSE)

set.seed(123)  # for reproducibility
model_lasso <- train(x=X_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
mean(model_lasso$resample$RMSE)
orig_mean <- round(mean(model_lasso$resample$RMSE),6)

stepdown_results = data.frame("Variable" = character(), 
                        "mean:RMSE" = numeric(),
                        "orig:RMSE" = numeric(),
                        "K or D" = character(),
                        stringsAsFactors=FALSE
)


# get list of test variables
varList <- c("x3332973","x3332974","x3332975","x3332976","x3332977","x3332978","x3332979","x3332980","x3332981","x3332982","x3332983","x3332984","x3332985","x3332986","x3332987","x3332988","x3332989","x3332990")

for (n in num.var) {
  print(n)
  XX_train[[n]] <- NULL
  X_train <- XX_train
  X_train <- data.matrix(XX_train)
  CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                   number=10,
                                   repeats=5,
                                   verboseIter=FALSE)
  
  set.seed(123)  # for reproducibility
  model_lasso <- train(x=X_train,y=y,
                       method="glmnet",
                       metric="RMSE",
                       maximize=FALSE,
                       trControl=CARET.TRAIN.CTRL,
                       tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                            lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                     0.00075,0.0005,0.0001)))
  mean(model_lasso$resample$RMSE)
  test_mean <- round(mean(model_lasso$resample$RMSE),6)
  if (test_mean > orig_mean) {
    outcome <- "KEEP"
  } 
  else {
    outcome <- "DROP"
  }
  stepdown_results[nrow(stepdown_results)+1, ] = c(as.character(n), 
                                                   test_mean,
                                                   orig_mean,
                                                   as.character(outcome))
  orig_mean <- test_mean
}
  