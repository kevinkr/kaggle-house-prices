
# Basic RPart Model ----------------------------------------------------------
# load previous code
#source("Models/4-data-prep.R")
library(partykit)
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)
library(pROC)
library(Metrics)

# for(attr in colnames(train))
# {
#   if (is.factor(train[[attr]]))
#   {
#     new.levels <- setdiff(levels(train[[attr]]), levels(test[[attr]]))
#     if ( length(new.levels) == 0 )
#     { print(paste(attr, '- no new levels')) }
#     else
#     {
#       print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
#       levels(test[[attr]]) <- union(levels(test[[attr]]), levels(train[[attr]]))
#     }
#   }
# }

#We'll use this later in the train function
preProc <-c("BoxCox", "center","scale")

seed <- 1469
testTrainData <- subset(testTrain, select = -c(SalePrice))

#Setting up sampling strategy
control <- trainControl(method = 'cv', number=10)

rpart_model <- train(x = testTrainData,
                     y = testTrain$SalePrice,
                     method="rpart",
                     preProcess = preProc,
                     metric = "RMSE",
                     trControl = control,
                     tuneLength = 30)

rpart_model$finalModel

fancyRpartPlot(rpart_model$finalModel, tweak=2)
plot(rpart_model, metric='RMSE')
ctreeVarImp = varImp(rpart_model)

# Prediction on hold out set
Prediction_1<- predict(rpart_model, newdata= validTrain)
rmse(log(validTrain$SalePrice),log(Prediction_1))

preds <- predict(rpart_model, newdata= test)

SUBMISSION_FILE = "Data/sample_submission.csv"
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$SalePrice = preds
write.csv(submission,file = 'Submissions/rpart-basic-v1-12-19-16.csv',row.names = FALSE)
