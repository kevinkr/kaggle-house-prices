library(xgboost)
library(data.table)
library(caret)
set.seed(1469)
TestX_train <- cbind(X_train,SalePrice=train$SalePrice)
rowsTrain <- createDataPartition(TestX_train$SalePrice
                                 , p = 0.8
                                 , list = FALSE)
testTrain <- TestX_train[rowsTrain,]
testY <- log(testTrain$SalePrice + 200)
testTrain <- subset(testTrain, select = -c(SalePrice))

validTrain <- TestX_train[-rowsTrain,]
validY <- log(validTrain$SalePrice + 200)
validTrain <- subset(validTrain, select = -c(SalePrice))


train <- as.data.table(testTrain)
trainTest <- as.data.table(validTrain)

test <- as.data.table(X_test)

y_train <- testY
y_trainTest <- validY

ntrain = nrow(train)
train_test = rbind(train, test)

features = names(train)

for (f in features) {
  if (class(train_test[[f]])=="factor" | class(train_test[[f]])=="ordered") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train_test[[f]])
    train_test[[f]] <- as.integer(factor(train_test[[f]], levels=levels))
  }
}

for (f in features) {
  if (class(trainTest[[f]])=="factor" | class(trainTest[[f]])=="ordered") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(trainTest[[f]])
    trainTest[[f]] <- as.integer(factor(trainTest[[f]], levels=levels))
  }
}

x_train = train_test[1:ntrain,]
x_test = train_test[(ntrain+1):nrow(train_test),]


dtrain = xgb.DMatrix(as.matrix(sapply(x_train, as.numeric)), label=y_train)
dtrainTest = xgb.DMatrix(as.matrix(sapply(trainTest, as.numeric)), label=y_trainTest)
#dtrainTestCV = xgb.DMatrix(as.matrix(trainTestCV), label=y_trainTestCV)
dtest = xgb.DMatrix(as.matrix(sapply(x_test, as.numeric)))

#rm(train,test,train_test,trainIndex,trainTestIndex,x_test,x_train)
gc()

amo.fairobj2 <- function(preds, dtrain) {
  
  labels <- getinfo(dtrain, "label")
  con <- 2
  x <- preds - labels
  grad <- con * x / (abs(x) + con)
  hess <- con ^ 2 / (abs(x) + con) ^ 2
  
  return(list(grad = grad, hess = hess))
  
}


amm_rmse <- function(preds , dtrain) {
  
  labels <- xgboost::getinfo(dtrain, "label") 
  elab <- as.numeric(labels) 
  epreds <- as.numeric(preds) 
  err <- rmse(exp(elab)+200, exp(epreds)+200)
  return(list(metric = "amm_rmse", value = round(err,4)))
  
}

tuner_rmse = data.frame("Rounds" = numeric(), 
                       "Depth" = numeric(),
                       "r_sample" = numeric(),
                       "c_sample" = numeric(), 
                       "minRMSE:Train" = numeric(),
                       "minRMSE:Test" = numeric(),
                       "best_round" = numeric(),
                       "eta" = numeric(),
                       "depth" = numeric(),
                       "min child weight" = numeric(),
                       "gamma" = numeric(),
                       "diff" = numeric()
)


xgb_params = list(
  nthread = 4,
  num_parallel_tree = 1
)

###### model complexity
# max_depth (default=6)
# min_child_weight (default=1)
# gamma (default=0)

##### add randomness to make training robust to noise
# subsample (default=1)
# colsample_bytree (default=1)
# increase eta (default=0.3) (and increase num_round when you increase eta)

for (gamma in c(0, 2, 4, 6)) {
for (min_cw in c(1, 3, 5)){
  
for (rounds in c(1000)){
  
  for (depth in c(1, 2, 6, 10)) {
    
    for (r_sample in c(0.8, 1)) {
      
      for (c_sample in c(0.2, 0.5, 1)) {
        
        for (eta_val in c(0.1, 0.2, 0.3)) {
          
          set.seed(1024)
          
          cv.res = xgb.cv(data = dtrain, 
                          nfold = 10, 
                          nrounds = rounds, 
                          eta = eta_val, 
                          max_depth = depth,
                          subsample = r_sample,
                          min_child_weight = min_cw,
                          colsample_bytree = c_sample,
                          early_stopping_rounds = 25,
                          print_every_n = 20,
                          eval_metric = 'rmse',
                          verbose = FALSE,
                          booster = 'gblinear',
                          #objective = amo.fairobj2,
                          maximize=FALSE)
          
          cv.res.log <- as.data.frame(cv.res$evaluation_log)
          
          
          print(paste(rounds, depth, r_sample, c_sample, eta_val, min_cw, gamma, 
                      round(min(cv.res.log[,4]),4), round(min(cv.res.log[,2]),4),
                      round(min(cv.res.log[,4]),4)-round(min(cv.res.log[,2]),4)))
                
          tuner_rmse[nrow(tuner_rmse)+1, ] = c(rounds, 
                                             depth, 
                                             r_sample, 
                                             c_sample, 
                                             min(cv.res.log[,2]), 
                                             min(cv.res.log[,4]),
                                             which.min(cv.res.log[,4]),
                                             eta_val,
                                             depth,
                                             min_cw,
                                             gamma,
                                             min(cv.res.log[,4])-min(cv.res.log[,2]))
          gc()
        }
      }
    }
  }
}
}
}

lowestRmse <- tuner_rmse[order(tuner_rmse$minRMSE.Test),][1,]
lowestRmse

resultsTableExport <- cbind(resultsTable,Model="xgboost",lowestRmse=lowestRmse)

xgb_params = list(
  colsample_bytree = lowestRmse$c_sample,
  subsample = lowestRmse$r_sample,
  eta = lowestRmse$eta,
  gamma = lowestRmse$gamma,
  max_depth = lowestRmse$depth, 
  min_child_weight = lowestRmse$min.child.weight, 
  nthread = 4,
  num_parallel_tree = 1,
  eval_metric = 'rmse' #evaluation metrics for validation data, 
)

set.seed(1024)

watchlist <- list('train' = dtrain, 'valid' = dtrainTest)

gbdt = xgb.train(xgb_params, 
                 dtrain, 
                 nrounds=10000, 
                 watchlist, 
                 maximize=FALSE, 
                 early_stopping_rounds=1000,
                 objective = amo.fairobj2,
                 print_every_n = 20)

gbdt_df <- as.data.frame(gbdt$evaluation_log)

ggplot() + 
  geom_line(data=gbdt_df, aes(x = iter, y = valid_rmse), color='green') +
  geom_line(data=gbdt_df, aes(x = iter, y = train_rmse), color='red')

#dtrainPred <- exp(predict(gbdt,dtrain))-200
#dtrainTestpred <- predict(gbdt,dtrainTest)
#cvpred <- as.data.frame(dtrainTestpred)
#cvpred <- as.data.frame(cbind(trainTestid,dtrainTestpred,y_trainTest))
#cvpred$y_trainTestCV <- exp(cvpred$y_trainTestCV)-200
#cvpred$y_trainTestCV <- cvpred$y_trainTestCV
#cvpred$error <-  cvpred$y_trainTestCV - cvpred$dtrainTestCVpred
#mae_val <- mae(cvpred$y_trainTestCV, cvpred$dtrainTestCVpred)

best_nrounds = gbdt$best_iteration
cv_rmse = gbdt$evaluation_log$valid_rmse[best_nrounds]
train_rmse = gbdt$evaluation_log$train_rmse[best_nrounds]
gbdt_df <- as.data.frame(gbdt$params)
cat(paste0('Model CV-RMSE: ',cv_rmse,' '," Train-RMSE: ", train_rmse, " best rounds:", best_nrounds))

resultsTableExport <- cbind(resultsTableExport,CV_rmse=cv_rmse)
currentDateTime <- strftime(Sys.time(), "%Y %m %d %H %M %S") 

csvFileName <- paste("C:/Users/kruegkj/Documents/GitHub/kaggle-house-prices/",
                     currentDateTime,".csv",sep="") 
write.csv(resultsTableExport, file=csvFileName) 
rm(resultsTableExport)
# ggplot() +
#   coord_cartesian(ylim = c(0, 12000)) +
#   geom_point(data=cvpred, aes(x = row.names(cvpred), y = y_trainTestCV), color='green') +
#   geom_point(data=cvpred, aes(x = row.names(cvpred), y = dtrainTestCVpred), color='red') +
#   geom_hline(yintercept=mae_val, color='black', size=1) +
#   geom_hline(yintercept=1098, color='blue', linetype=3, size = 1) +
#   ggtitle("Predicted vs. Expected Loss on Holdout Set") +
#   labs(x="row",y="Loss") +
#   theme(plot.title = element_text(family = "Arial", color="#666666", face="bold", size=16, hjust=0)) +
#   theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=16)) 
# 
# apply(cvpred,2, sum)

xgb.model1 = xgb.cv(xgb_params,
                    dtrain,
                    #objective = 'reg:linear',
                    objective = amo.fairobj2,
                    nfold=10,
                    nrounds=10000,
                    early_stopping_rounds=25,
                    print_every_n = 20,
                    verbose= 1,
                    maximize=FALSE)

#xgb.model1$evaluation_log
#xgb.model1_df <- as.data.frame(xgb.model1$evaluation_log)

best_nrounds1 = xgb.model1$best_iteration
cv_mean1 = xgb.model1$evaluation_log$test_rmse_mean[best_nrounds1]
cv_std1 = xgb.model1$evaluation_log$test_rmse_std[best_nrounds1]
as.data.frame(xgb.model1$params)
cat(paste0('Model1 CV-Mean: ',cv_mean1,' '," CV-Std: ", cv_std1, " best rounds:", best_nrounds1))



importance_matrix <- xgb.importance(feature_names = names(fullSet), model = gbdt)
xgb.plot.importance(importance_matrix[1:20,])

submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = exp(predict(gbdt,dtest))-200
write.csv(submission,file = 'Submissions/xgb-custom-v8-1-10-17.csv',row.names = FALSE)



