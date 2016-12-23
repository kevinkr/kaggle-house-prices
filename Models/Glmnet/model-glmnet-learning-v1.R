# House prices Kaggle competition
# Start: 12-26-16
library(glmnet)

# Based on http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
test <- subset(test, select = -c(SalePrice))
test.m <- data.matrix(test)

subTrain.m <- data.matrix(train)
subTrain.y <- data.matrix(train$SalePrice)

subTest.m <- data.matrix(subTest[,1:131])
subTest.y <- data.matrix(subTest$loss)

# fit model
fit = glmnet(subTrain.m, subTrain.y)
# summarize the fit
summary(fit)
plot(fit)

cv.fit <- cv.glmnet(subTrain.m,subTrain.y)
plot(cv.fit)

test.pred <- predict(fit,newx=subTest.m,s=cv.fit$lambda.min)
mte <- apply((test.pred-subTest.y)^2,2,mean)

#points(log(fit$lambda),mte,col="blue",pch="*")
#legend("topleft",legend=c("10 fold CV","Test"),pch="*",col=c("red","blue"))

plot(fit,xvar="lambda")
plot(fit,xvar="dev")

# See http://stats.stackexchange.com/questions/188753/lasso-regression-for-predicting-continuous-variable-variable-selection
plot(fit, xvar = "lambda")


predictions <- predict(fit,newx=test.m,s=cv.fit$lambda.min)
test.df <- as.data.frame(test.m)
test.df$predictionColumn<-c(predictions)


#pred.df <-as.data.frame(t(predictions))

solution <- data.frame(id = test.df$Id, SalePrice = test.df$predictionColumn)

# Write the solution to file
write.csv(solution, file = 'Submissions/glmnet-learning-12-26-16-1.csv', row.names = F)
