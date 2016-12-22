# Kaggle house prices
# MLR Rpart
library(mlr)
library(partykit)
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)
library(pROC)
library(Metrics)

#' @export
makeRLearner.regr.rpart = function() {
  makeRLearnerRegr(
    cl = "regr.rpart",
    package = "rpart",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "minsplit", default = 20L, lower = 1L),
      makeIntegerLearnerParam(id = "minbucket", lower = 1L),
      makeNumericLearnerParam(id = "cp", default = 0.01, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "maxcompete", default = 4L, lower = 0L),
      makeIntegerLearnerParam(id = "maxsurrogate", default = 5L, lower = 0L),
      makeDiscreteLearnerParam(id = "usesurrogate", default = 2L, values = 0:2),
      makeDiscreteLearnerParam(id = "surrogatestyle", default = 0L, values = 0:1),
      # we use 30 as upper limit, see docs of rpart.control
      makeIntegerLearnerParam(id = "maxdepth", default = 30L, lower = 1L, upper = 30L),
      makeIntegerLearnerParam(id = "xval", default = 10L, lower = 0L, tunable = FALSE)
    ),
    par.vals = list(xval = 0L),
    properties = c("missings", "numerics", "factors", "ordered", "weights", "featimp"),
    name = "Decision Tree",
    short.name = "rpart",
    note = "`xval` has been set to `0` by default for speed."
  )
}

#' @export
trainLearner.regr.rpart = function(.learner, .task, .subset, .weights = NULL,  ...) {
  d = getTaskData(.task, .subset)
  if (is.null(.weights)) {
    f = getTaskFormula(.task)
    rpart::rpart(f, data = d, ...)
  } else  {
    f = getTaskFormula(.task)
    rpart::rpart(f, data = d, weights = .weights, ...)
  }
}

#' @export
predictLearner.regr.rpart = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, newdata = .newdata, ...)
}

#' @export
getFeatureImportanceLearner.regr.rpart = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.rpart(.learner, .model, ...)
}

trainTask = makeRegrTask(data = as.data.frame(testTrain), target = "SalePrice")
testTask = makeRegrTask(data = as.data.frame(test), target = "SalePrice")

# specify mlr learner with some nice hyperpars
set.seed(123)
lrn = makeLearner("regr.rpart")
# get list of params
#print(lrn$par.set)
# get current set of params
#print(lrn$par.vals)
getDefaultMeasure(trainTask)

## This is how you could do hyperparameter tuning with random search
# 1) Define the set of parameters you want to tune (here we use only 'obj_par')
ps <- makeParamSet(
      makeIntegerParam("minsplit", lower = 1, upper = 40),
      makeIntegerParam("minbucket", lower = 1, upper = 40),
      makeNumericParam("cp", lower = 0.0002, upper = 0.05, trafo = function(x) x/2),
      makeIntegerParam("maxdepth", lower = 1, upper = 30),
      makeDiscreteParam("maxsurrogate", values = c(0, 5, 10)),
      makeDiscreteParam("usesurrogate", values = c(0, 1, 2))
) 

# 2) Use 10-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 5L, predict = "both")

# 3) Here we use random search (with 5 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 10)

# 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters in parallel
#parallelStartMulticore(5)
#res = tuneParams(lrn, task = trainTask, resampling = rdesc,
#                 par.set = ps, control = ctrl)
# Measures we'd like to use to evaluate
m1 = rmse
m2 = setAggregation(rmse, train.rmse)

res = tuneParams(lrn, 
                 task = trainTask, 
                 resampling = rdesc,
                 par.set = ps, 
                 control = makeTuneControlGrid(resolution = 8L),
                 measures = m1)

opt.grid = as.data.frame(res$opt.path)
# Train on entire dataset (using best hyperparameters)
lrn = setHyperPars(lrn, par.vals = res$x)
mod = train(lrn, trainTask)
plotLearnerPrediction("regr.rpart", features = "LotFrontage", task = trainTask)

# predict on new data
predict = predict(mod, newdata = validTrain)
predict
rmse(log(validTrain$SalePrice),log(as.data.frame(predict)))

# train on full trian set
fullTrainTask = makeRegrTask(data = as.data.frame(train), target = "SalePrice")
final_mod = train(lrn, fullTrainTask)

pred = getPredictionResponse(predict(final_mod, testTask))
summary(pred)

SUBMISSION_FILE = "Data/sample_submission.csv"
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$SalePrice = pred
write.csv(submission,file = 'Submissions/rpart-mlr-v6-12-21-16.csv',row.names = FALSE)
