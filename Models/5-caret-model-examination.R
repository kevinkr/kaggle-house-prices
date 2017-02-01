# Kaggle home prices
# All in one caret analysis with validation data set
library(caret)

cat.var <- names(X_train)[which(sapply(X_train, is.factor))]
X_train <- X_train %>% mutate_each_(funs(factor), cat.var)

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
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=10,
                                 repeats=5,
                                 verboseIter=FALSE)
set.seed(123)  # for reproducibility

##############################
# Linear models
##########
# GLM
glm.caret <- train(x=testTrain, y=testY,
                   method="glm",
                   trControl=CARET.TRAIN.CTRL, 
                   maximize=FALSE,
                   preProc = c("center", "scale"),
                   metric="RMSE") 

# Predict using the test data
pred <- predict(glm.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('GLM')
# Print, plot variable importance
print(varImp(glm.caret, scale = FALSE))
plot(varImp(glm.caret, scale = FALSE), main="Variable Importance using GLM")
summary(glm.caret)

##################################
# Partial Least Squared
pls.caret <- train(x=testTrain, y=testY,
                   method="pls",
                   trControl=CARET.TRAIN.CTRL, 
                   maximize=FALSE,
                   tuneLength = 20,
                   preProc = c("center", "scale"),
                   metric="RMSE") 

# Predict using the test data
pred <- predict(pls.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('PLS')
# Print, plot variable importance
print(varImp(pls.caret, scale = FALSE))
plot(varImp(pls.caret, scale = FALSE), main="Variable Importance using PLS")
summary(pls.caret)
plot(pls.caret)

##################################
# GLMNET - Ridge
# 
lambdas <- seq(1,0,-0.001)

# train model	
glmnet.ridge.caret <- train(x=testTrain,y=testY,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))

ggplot(data=filter(glmnet.ridge.caret$result,RMSE<0.13)) +
  geom_line(aes(x=lambda,y=RMSE))

# Predict using the test data
pred <- predict(glmnet.ridge.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('GLMNET - Ridge')
# Print, plot variable importance
print(varImp(glmnet.ridge.caret, scale = FALSE))
plot(varImp(glmnet.ridge.caret, scale = FALSE), main="Variable Importance using PLS")
summary(glmnet.ridge.caret)

############################
# GLMNET - Lasso

# train model
glmnet.lasso.caret <- train(x=testTrain,y=testY,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))

ggplot(data=filter(glmnet.lasso.caret$result,RMSE<0.13)) +
  geom_line(aes(x=lambda,y=RMSE))

# Predict using the test data
pred <- predict(glmnet.lasso.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('GLMNET - Lasso')
# Print, plot variable importance
print(varImp(glmnet.lasso.caret, scale = FALSE))
plot(varImp(glmnet.lasso.caret, scale = FALSE), main="Variable Importance using GLMNET - Lasso")
summary(glmnet.lasso.caret)

#################################
# Ridge regression
ridgeGrid <- data.frame(.lambda = seq(0, .1, length = 15))

ridge.caret <- train(x=testTrain,y=testY,
                     method="ridge",
                     trControl=CARET.TRAIN.CTRL, 
                     maximize=FALSE,
                     tuneGrid = ridgeGrid,
                     preProc = c("center", "scale"),
                     metric="RMSE") 

# Predict using the test data
pred <- predict(ridge.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('Ridge')
# Print, plot variable importance
print(varImp(ridge.caret, scale = FALSE))
plot(varImp(ridge.caret, scale = FALSE), main="Variable Importance using Ridge")
summary(ridge.caret)
plot(ridge.caret)

#############################
# Lasso

lassoGrid <- expand.grid(.fraction = seq(.05, 1, length = 20))

lasso.caret <- train(x=testTrain,y=testY,
                     method="lasso",
                     trControl=CARET.TRAIN.CTRL, 
                     maximize=FALSE,
                     tuneGrid = lassoGrid,
                     preProc = c("center", "scale"),
                     metric="RMSE") 

# Predict using the test data
pred <- predict(lasso.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('Ridge')
# Print, plot variable importance
print(varImp(lasso.caret, scale = FALSE))
plot(varImp(lasso.caret, scale = FALSE), main="Variable Importance using Ridge")
summary(lasso.caret)
plot(lasso.caret)


###########################
# Elastic Net
enetGrid <- expand.grid(.lambda = c(0, 0.01, .1),
                        .fraction = seq(.05, 1, length = 20))

enet.caret <- train(x=testTrain,y=testY,
                    method="enet",
                    trControl=CARET.TRAIN.CTRL, 
                    maximize=FALSE,
                    tuneGrid = enetGrid,
                    preProc = c("center", "scale"),
                    metric="RMSE") 

# Predict using the test data
pred <- predict(enet.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('Elastic Net')
# Print, plot variable importance
print(varImp(enet.caret, scale = FALSE))
plot(varImp(enet.caret, scale = FALSE), main="Variable Importance using Elastic Net")
summary(enet.caret)
plot(enet.caret)

########################################
# Bayes GLM
bayesglm.caret <- train(x=testTrain,y=testY,
                        method="bayesglm",
                        trControl=CARET.TRAIN.CTRL, 
                        maximize=FALSE,
                        preProc = c("center", "scale"),
                        metric="RMSE") 

# Predict using the test data
pred <- predict(bayesglm.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('Bayes GLM')
# Print, plot variable importance
print(varImp(bayesglm.caret, scale = FALSE))
plot(varImp(bayesglm.caret, scale = FALSE), main="Variable Importance using Bayes GLM")
summary(bayesglm.caret)

###############################
# MARS
marsGrid <- expand.grid(.degree = seq(5,10),
                        .nprune = (4:20) * 2)

mars.caret <- train(x=testTrain,y=testY,
                    method="earth",
                    trControl=CARET.TRAIN.CTRL, 
                    maximize=FALSE,
                    tuneGrid = marsGrid,
                    metric="RMSE") 

# Predict using the test data
pred <- predict(mars.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('MARS')
# Print, plot variable importance
print(varImp(mars.caret, scale = FALSE))
plot(varImp(mars.caret, scale = FALSE), main="Variable Importance using MARS")
summary(mars.caret)

####################################
# Rpart

rpart.caret <- train(x=testTrain,y=testY,
                     method="rpart",
                     preProc = c("center", "scale"),
                     metric = "RMSE",
                     trControl = CARET.TRAIN.CTRL,
                     tuneLength = 30)

# Predict using the test data
pred <- predict(rpart.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('RPart')
# Print, plot variable importance
print(varImp(rpart.caret, scale = FALSE))
plot(varImp(rpart.caret, scale = FALSE), main="Variable Importance using RPart")
summary(rpart.caret)

###########################
# Ctree
Grid <- expand.grid(mincriterion = seq(0, 0.95, 0.05))

# train model
ctree.caret <- train(x=testTrain,y=testY,
                     method="ctree",
                     metric="RMSE",
                     maximize=FALSE,
                     preProc = c("center", "scale"),
                     tuneGrid=Grid,
                     trControl=CARET.TRAIN.CTRL)

# Predict using the test data
pred <- predict(ctree.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('CTree')
# Print, plot variable importance
print(varImp(ctree.caret, scale = FALSE))
plot(varImp(ctree.caret, scale = FALSE), main="Variable Importance using CTree")
summary(ctree.caret)

##################################
# CForest
Grid <- expand.grid(mtry = seq(100, 108, 2))

# train model
set.seed(123)  # for reproducibility
cforest.caret <- train(x=testTrain,y=testY,
                       method="cforest",
                       metric="RMSE",
                       maximize=FALSE,
                       tuneGrid=Grid,
                       preProc = c("center", "scale"),
                       trControl=CARET.TRAIN.CTRL)

# Predict using the test data
pred <- predict(cforest.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('CForest')
# Print, plot variable importance
print(varImp(cforest.caret, scale = FALSE))
plot(varImp(cforest.caret, scale = FALSE), main="Variable Importance using CForest")
summary(cforest.caret)

##################################
# GBM
caretGrid <- expand.grid(interaction.depth=c(5, 7), 
                         n.trees = (0:70)*50,
                         shrinkage=c(0.01, 0.001),
                         n.minobsinnode=c(5, 10, 20))

set.seed(99)

gbm.caret <- train(x=testTrain,y=testY,
                   distribution="gaussian", 
                   method="gbm",
                   trControl=CARET.TRAIN.CTRL, 
                   verbose=FALSE, 
                   tuneGrid=caretGrid, 
                   maximize=FALSE,
                   metric="RMSE", 
                   preProc = c("center", "scale"),
                   bag.fraction=0.5)  

# Predict using the test data
pred <- predict(gbm.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('GBM')
# Print, plot variable importance
print(varImp(gbm.caret, scale = FALSE))
plot(varImp(gbm.caret, scale = FALSE), main="Variable Importance using GBM")
summary(gbm.caret)

############################
# SVM Radial
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25)
)

svm.radial.caret <- train(x=testTrain,y=testY,
                          method="svmRadial",
                          trControl=CARET.TRAIN.CTRL,
                          tuneGrid = grid,
                          maximize=FALSE,
                          preProc = c("center", "scale"),
                          metric="RMSE")  

# Predict using the test data
pred <- predict(svm.radial.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('SVM Radial')
# Print, plot variable importance
print(varImp(svm.radial.caret, scale = FALSE))
plot(varImp(svm.radial.caret, scale = FALSE), main="Variable Importance using SVM Radial")
summary(svm.radial.caret)
###########################
# SVM Linear
svm.linear.caret <- train(x=testTrain,y=testY,
                          method="svmLinear",
                          trControl=CARET.TRAIN.CTRL,
                          maximize=FALSE,
                          preProc = c("center", "scale"),
                          metric="RMSE") 

# Predict using the test data
pred <- predict(svm.linear.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('SVM Linear')
# Print, plot variable importance
print(varImp(svm.linear.caret, scale = FALSE))
plot(varImp(svm.linear.caret, scale = FALSE), main="Variable Importance using SVM Linear")
summary(svm.linear.caret)
############################
# SVM Linear3
grid <- expand.grid(Loss = c("L1", "L2"),
                    cost = c(0.001, 0.01, 0.05, 0.1, 0.25, 1, 2, 
                             4, 6, 8, 10, 15, 20, 30, 50, 75, 100))

svm.linear3.caret <- train(x=testTrain,y=testY,
                           method="svmLinear3",
                           trControl=CARET.TRAIN.CTRL,
                           maximize=FALSE,
                           tuneGrid = grid,
                           preProc = c("center", "scale"),
                           svr_eps = c(0.01, 0.25, 0.5, 1, 2, 3),
                           metric="RMSE") 

# Predict using the test data
pred <- predict(svm.linear3.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('SVM Linear3')
# Print, plot variable importance
print(varImp(svm.linear3.caret, scale = FALSE))
plot(varImp(svm.linear3.caret, scale = FALSE), main="Variable Importance using SVM Linear3")
summary(svm.linear3.caret)

##########################
# GAMBoost
gamBoostGrid = expand.grid(mstop = seq(100, 200, 100),
                           prune = c('yes', 'no'))


gamboost.caret <- train(x=testTrain,y=testY,
                        method="gamboost",
                        trControl=CARET.TRAIN.CTRL, 
                        maximize=FALSE,
                        preProc = c("center", "scale"),
                        tuneGrid = gamBoostGrid,
                        metric="RMSE") 

# Predict using the test data
pred <- predict(gamboost.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('GAMBoost')
# Print, plot variable importance
print(varImp(gamboost.caret, scale = FALSE))
plot(varImp(gamboost.caret, scale = FALSE), main="Variable Importance using GAMBoost")
summary(gamboost.caret)

###################################
# GLMBoost
glmBoostGrid = expand.grid(mstop = seq(600, 3000, 200),
                           prune = c('yes', 'no'))

glmboost.caret <- train(x=testTrain,y=testY,
                        method="glmboost",
                        trControl=CARET.TRAIN.CTRL, 
                        maximize=FALSE,
                        preProc = c("center", "scale"),
                        tuneGrid = glmBoostGrid,
                        metric="RMSE") 

# Predict using the test data
pred <- predict(glmboost.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('GLMBoost')
# Print, plot variable importance
print(varImp(glmboost.caret, scale = FALSE))
plot(varImp(glmboost.caret, scale = FALSE), main="Variable Importance using GLMBoost")
summary(glmboost.caret)

###################################
# KNN
knn.caret <- train(x=testTrain,y=testY,
                        method="knn",
                        trControl=CARET.TRAIN.CTRL, 
                        maximize=FALSE,
                        preProc = c("center", "scale"),
                        metric="RMSE") 

# Predict using the test data
pred <- predict(knn.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('KNN')
# Print, plot variable importance
print(varImp(knn.caret, scale = FALSE))
plot(varImp(knn.caret, scale = FALSE), main="Variable Importance using KNN")
summary(knn.caret)

###############################
# Random Forest
tunegrid <- expand.grid(.mtry=c(1:15))

rf.caret <- train(x=testTrain,y=testY,
                  method="rf",
                  trControl=CARET.TRAIN.CTRL, 
                  maximize=FALSE,
                  tuneGrid = tunegrid,
                  preProc = c("center", "scale"),
                  metric="RMSE") 

# Predict using the test data
pred <- predict(rf.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('RF')
# Print, plot variable importance
print(varImp(rf.caret, scale = FALSE))
plot(varImp(rf.caret, scale = FALSE), main="Variable Importance using RF")
summary(rf.caret)

###############################
# Neural Net
tune.grid <- expand.grid(.decay = c(0.1, 0.01, 0.0075), .size = c(1, 10))

nn.caret <- train(x=testTrain,y=testY,
                  method="nnet",
                  trControl=CARET.TRAIN.CTRL, 
                  maximize=FALSE,
                  metric="RMSE",
                  maxit = 500,
                  tuneGrid = tune.grid,
                  linout = TRUE,
                  MaxNWts = 10 * (ncol(testTrain) + 1) + 10 + 1,
                  trace = FALSE) 

# Predict using the test data
pred <- predict(nn.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('Neural Net')
# Print, plot variable importance
print(varImp(nn.caret, scale = FALSE))
plot(varImp(nn.caret, scale = FALSE), main="Variable Importance using Neural Net")
summary(nn.caret)

##################################
# XGBoost
tune.grid = expand.grid(nrounds = 1000,
                        eta = c(0.01),
                        max_depth = c(1),
                        gamma = c(1),
                        colsample_bytree = c(0.1, 0.3, 0.5),
                        min_child_weight = c(1, 3),
                        subsample = c(1)
                        )

xgbTree.caret <- train(x=testTrain,y=testY,
                       method = "xgbTree",
                       tuneGrid = tune.grid,                            
                       trControl = CARET.TRAIN.CTRL,
                       maximize=FALSE,
                       preProc = c("center", "scale"),
                       metric="RMSE")

# Predict using the test data
pred <- predict(xgbTree.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('XGBTree')
# Print, plot variable importance
print(varImp(xgbTree.caret, scale = FALSE))
plot(varImp(xgbTree.caret, scale = FALSE), main="Variable Importance using XGBTree")
summary(xgbTree.caret)

##################################
# XGBLinear
tune.grid = expand.grid(nrounds = 1000,
                        lambda = c(0.01),
                        alpha = c(1),
                        eta = c(0.1)
)

xgbLinear.caret <- train(x=testTrain,y=testY,
                       method = "xgbLinear",
                       tuneGrid = tune.grid,                            
                       trControl = CARET.TRAIN.CTRL,
                       maximize=FALSE,
                       preProc = c("center", "scale"),
                       metric="RMSE")

# Predict using the test data
pred <- predict(xgbLinear.caret, validTrain)

my_data=as.data.frame(cbind(predicted=pred,observed=validY))
ggplot(my_data,aes(predicted,observed))+geom_point()+geom_smooth(method=lm)+ggtitle('XGBLinear')
# Print, plot variable importance
print(varImp(xgbLinear.caret, scale = FALSE))
plot(varImp(xgbLinear.caret, scale = FALSE), main="Variable Importance using XGBLinear")
summary(xgbLinear.caret)

###############################
###############################
# collect resamples

results <- resamples(list(GLM=glm.caret, PLS=pls.caret, GLMNETRidge=glmnet.ridge.caret,
                          GLMNETLasso=glmnet.lasso.caret, Ridge=ridge.caret, Lasso=lasso.caret,
                          ElasticNet=enet.caret, BayesGLM=bayesglm.caret, 
                          RPart=rpart.caret, CTree=ctree.caret, CForest=cforest.caret,
                          GBM=gbm.caret, SVMRadial=svm.radial.caret, SVMLinear=svm.linear.caret,
                          SVMLinear3=svm.linear3.caret, MARS=mars.caret, GLMBoost=glmboost.caret,
                          KNN=knn.caret, RF=rf.caret, NeuralNet=nn.caret, XGBTree=xgbTree.caret,
                          XGBLinear=xgbLinear.caret))
# summarize the distributions
summary(results)
# boxplots of results
bwplot(results, scales = 'free')
# dot plots of results
dotplot(results, scales = 'free')
