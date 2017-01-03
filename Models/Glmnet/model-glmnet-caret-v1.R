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


# test out Ridge regression model

lambdas <- seq(1,0,-0.001)

# train model
set.seed(123)  # for reproducibility
model_ridge <- train(x=X_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=0, # Ridge regression
                                          lambda=lambdas))


ggplot(data=filter(model_ridge$result,RMSE<0.15)) +
  geom_line(aes(x=lambda,y=RMSE))

mean(model_ridge$resample$RMSE)

# test out Lasso regression model

# train model
set.seed(123)  # for reproducibility
model_lasso <- train(x=X_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
#model_lasso

mean(model_lasso$resample$RMSE)


# extract coefficients for the best performing model
coef <- data.frame(coef.name = dimnames(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda)))

# exclude the (Intercept) term
coef <- coef[-1,]

# print summary of model results
picked_features <- nrow(filter(coef,coef.value!=0))
not_picked_features <- nrow(filter(coef,coef.value==0))

cat("Lasso picked",picked_features,"variables and eliminated the other",
    not_picked_features,"variables\n")

# sort coefficients in ascending order
coef <- arrange(coef,-coef.value)

# extract the top 10 and bottom 10 features
imp_coef <- rbind(head(coef,10),
                  tail(coef,10))

ggplot(imp_coef) +
  geom_bar(aes(x=reorder(coef.name,coef.value),y=coef.value),
           stat="identity") +
  ylim(-1.5,0.6) +
  coord_flip() +
  ggtitle("Coefficents in the Lasso Model") +
  theme(axis.title=element_blank())

# make create submission file
preds <- exp(predict(model_lasso,newdata=X_test)) - 1

# construct data frame for solution
submission = read.csv("Data/sample_submission.csv", colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission, "Submissions/caret-glmnet-v3-1-13-17.csv", row.names = FALSE)
