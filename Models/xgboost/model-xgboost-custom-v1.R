library(xgboost)
library(data.table)

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


dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtrainTest = xgb.DMatrix(as.matrix(trainTest), label=y_trainTest)
#dtrainTestCV = xgb.DMatrix(as.matrix(trainTestCV), label=y_trainTestCV)
dtest = xgb.DMatrix(as.matrix(x_test))

rm(train,test,train_test,trainIndex,trainTestIndex,x_test,x_train)
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

# number of trees = rounds


gamma = 10
for (gamma in c(2, 4, 6)) {
for (min_cw in c(2, 4, 6)){
  
for (rounds in c(1500, 1000)){
  
  for (depth in seq(1, 4, 1)) {
    
    for (r_sample in c(.6, .7)) {
      
      for (c_sample in c(.6, .8)) {
        
        for (eta_val in c(0.0125, .025, .05)) {
          
          set.seed(1024)
          
          cv.res = xgb.cv(data = dtrain, 
                          nfold = 5, 
                          nrounds = rounds, 
                          eta = eta_val, 
                          max_depth = depth,
                          subsample = r_sample,
                          min_child_weight = min_cw,
                          colsample_bytree = c_sample,
                          early_stopping_rounds = 25,
                          print_every_n = 20,
                          eval_metric = 'rmse',
                          #feval = amm_rmse,
                          verbose = FALSE,
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


train_rmse = data.frame("Rounds" = numeric(), 
                        "Depth" = numeric(),
                        "r_sample" = numeric(),
                        "c_sample" = numeric(), 
                        "minRMSE:Train" = numeric(),
                        "best_round" = numeric(),
                        "eta" = numeric(),
                        "depth" = numeric(),
                        "min child weight" = numeric(),
                        "gamma" = numeric()
)
watchlist <- list('valid' = dtrain, 'train' = dtrainTest)

for (gamma in c(0,2)) {
  for (min_cw in c(2, 10, 20)){
    
    for (rounds in c(2000)){
      
      for (depth in seq(1, 6, 1)) {
        
        for (r_sample in c(.2, .6, .7)) {
          
          for (c_sample in c(.4, .6, .8)) {
            
            for (eta_val in c(.2, .3)) {
              
              xgb_params = list(
                colsample_bytree = c_sample,
                subsample = r_sample, 
                eta = eta_val,
                gamma = gamma,
                max_depth = depth,
                min_child_weight = min_cw,
                nthread = 4,
                num_parallel_tree = 1,
                eval_metric = 'rmse' #evaluation metrics for validation data, 
              )
              set.seed(1024)
              
              train.res = xgb.train(xgb_params,
                                    dtrain, 
                                    nrounds = rounds, 
                                    early_stopping_rounds = 50,
                                    print_every_n = 20,
                                    watchlist,
                                    verbose = FALSE,
                                    maximize=FALSE)
              
              train.res.log <- as.data.frame(train.res$evaluation_log)
              #best_nrounds = train.res$best_iteration
              #valid_rmse = train.res.log$valid_rmse[best_nrounds]
              #train_rmse = train.res.log$train_rmse[best_nrounds]
              
              print(paste(rounds, depth, r_sample, c_sample, eta_val, min_cw, gamma, train.res$best_iteration, train.res$best_score  ))
              train_rmse[nrow(train_rmse)+1, ] = c(rounds,
                                                   depth,
                                                   r_sample,
                                                   c_sample,
                                                   train.res$best_score,
                                                   train.res$best_iteration,
                                                   eta_val,
                                                   depth,
                                                   min_cw,
                                                   gamma)
              gc()
            }
          }
        }
      }
    }
  }
}

xgb_params = list(
  colsample_bytree = .6,
  subsample = .6, #.8 1821 .6 1822 .4 1821
  eta = .025, #0.01
  #alpha = 8,
  gamma = 2,
  #lambda = .185,
  max_depth = 1, #10-12,1832 14,1833 8,1832 4,1817 15,1833
  min_child_weight = 6, #4-5,10  1817
  nthread = 4,
  num_parallel_tree = 1,
  eval_metric = 'rmse' #evaluation metrics for validation data, 
)

set.seed(1024)

watchlist <- list('valid' = dtrainTest, 'train' = dtrain)

gbdt = xgb.train(xgb_params, 
                 dtrain, 
                 nrounds=10000, 
                 watchlist, 
                 maximize=FALSE, 
                 early_stopping_rounds=10,
                 print_every_n = 20)

gbdt_df <- as.data.frame(gbdt$evaluation_log)

ggplot() + 
  geom_line(data=gbdt_df, aes(x = iter, y = valid_rmse), color='green') +
  geom_line(data=gbdt_df, aes(x = iter, y = train_rmse), color='red')

#dtrainTestCVpred <- exp(predict(gbdt,dtrainTestCV))-200
#dtrainTestCVpred <- predict(gbdt,dtrainTestCV)
#cvpred <- as.data.frame(dtrainTestCVpred)
#cvpred <- as.data.frame(cbind(trainTestCVid,dtrainTestCVpred,y_trainTestCV))
#cvpred$y_trainTestCV <- exp(cvpred$y_trainTestCV)-200
#cvpred$y_trainTestCV <- cvpred$y_trainTestCV
#cvpred$error <-  cvpred$y_trainTestCV - cvpred$dtrainTestCVpred
#mae_val <- mae(cvpred$y_trainTestCV, cvpred$dtrainTestCVpred)

best_nrounds = gbdt$best_iteration
cv_rmse = gbdt$evaluation_log$valid_rmse[best_nrounds]
train_rmse = gbdt$evaluation_log$train_rmse[best_nrounds]
as.data.frame(gbdt$params)
cat(paste0('Model CV-RMSE: ',cv_rmse,' '," Train-RMSE: ", train_rmse, " best rounds:", best_nrounds))

ggplot() +
  coord_cartesian(ylim = c(0, 12000)) +
  geom_point(data=cvpred, aes(x = row.names(cvpred), y = y_trainTestCV), color='green') +
  geom_point(data=cvpred, aes(x = row.names(cvpred), y = dtrainTestCVpred), color='red') +
  geom_hline(yintercept=mae_val, color='black', size=1) +
  geom_hline(yintercept=1098, color='blue', linetype=3, size = 1) +
  ggtitle("Predicted vs. Expected Loss on Holdout Set") +
  labs(x="row",y="Loss") +
  theme(plot.title = element_text(family = "Arial", color="#666666", face="bold", size=16, hjust=0)) +
  theme(axis.title = element_text(family = "Arial", color="#666666", face="bold", size=16)) 

apply(cvpred,2, sum)

xgb.model1 = xgb.cv(xgb_params,
                    dtrain,
                    objective = 'reg:linear',
                    nfold=10,
                    nrounds=1000,
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



importance_matrix <- xgb.importance(feature_names = names(train), model = gbdt)
xgb.plot.importance(importance_matrix[1:20,])

submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = exp(predict(gbdt,dtest))-200
write.csv(submission,file = 'Submissions/xgb-custom-v2-1-6-17.csv',row.names = FALSE)
