# Kaggle Home Prices step wise addition variable evaluation
source("Code/3-1-feateng-full.R")
source("Code/3-feature-q-1way-2way-3way.R")

# add 2 way and 3 way interaction here

# then do data prep

library(caret)
train$SalePrice <- log(train$SalePrice + 200)
y <- train$SalePrice

stepdown_results = data.frame("Variable" = character(), 
                              "mean:RMSE" = numeric(),
                              "orig:RMSE" = numeric(),
                              "K or D" = character(),
                              stringsAsFactors=FALSE
)

set.seed(123)  # for reproducibility

# set first variables for selection and testing
XX_train <- subset(X_train, select = c(OverallCondCubed, OverallCondExp))

# drop them from X_train
X_train <- subset(X_train, select = -c(OverallCondCubed, OverallCondExp))


CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=5,
                                 verboseIter=FALSE)

model_lasso <- train(x=data.matrix(XX_train),y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
mean(model_lasso$resample$RMSE)
orig_mean <- round(mean(model_lasso$resample$RMSE),6)


# get list of all var names in X_train
col.var <- sort(colnames(X_train),decreasing = TRUE)
#col.var <- colnames(X_train)

# randomize list
col.var <- col.var[sample(1:nrow(col.var)), ]

for (n in col.var) {
  print(n)
  name = as.name(n)
  XX_train <- cbind(XX_train, name=X_train[[n]])
  names(XX_train)[ncol(XX_train)] <- n
  
  model_lasso <- train(x=data.matrix(XX_train),y=y,
                       method="glmnet",
                       metric="RMSE",
                       maximize=FALSE,
                       trControl=CARET.TRAIN.CTRL,
                       tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                            lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                     0.00075,0.0005,0.0001)))
  mean(model_lasso$resample$RMSE)
  test_mean <- round(mean(model_lasso$resample$RMSE),6)
  if (test_mean < orig_mean) {
    outcome <- "KEEP"
    stepdown_results[nrow(stepdown_results)+1, ] = c(as.character(n), 
                                                     test_mean,
                                                     orig_mean,
                                                     as.character(outcome))
    orig_mean <- test_mean
  } 
  else {
    outcome <- "DROP"
    XX_train[[n]] <- NULL
    stepdown_results[nrow(stepdown_results)+1, ] = c(as.character(n), 
                                                     test_mean,
                                                     orig_mean,
                                                     as.character(outcome))
  }
  
}
