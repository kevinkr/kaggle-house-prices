
# Basic LM Model ----------------------------------------------------------
# load previous code
#source("Models/4-data-prep.R")

ctrl<-trainControl(method = "repeatedcv",number = 10,repeats=3)
seed <- 1469
metric <- "RMSE"

lmCVFit <- train(SalePrice ~ ., data = testTrain, method = "lm", trControl = ctrl, metric="Rsquared")
summary(lmCVFit)

residuals<-resid(lmCVFit)

predictedValues<-predict(lmCVFit)

plot(testTrain$SalePrice,residuals)

abline(0,0)

plot(testTrain$SalePrice,predictedValues)

# vaiable Importance
varImp(lmCVFit)

plot(varImp(lmCVFit))

# prediction
predictedVal <- predict(lmCVFit, validTrain)

new.modelvalues <-data.frame(obs = validTrain$SalePrice, pred=predictedVal)

defaultSummary(new.modelvalues)

xyplot(new.modelvalues$obs ~ new.modelvalues$pred,
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Observed")

xyplot(resid(lmCVFit) ~ predict(lmCVFit),
       type = c("p", "g"),
       xlab = "Predicted", ylab = "Residuals")

