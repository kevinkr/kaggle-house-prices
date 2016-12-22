# Kaggle House Prices 12-21-16
# MLR Ranger

library(mlr)
library(mlbench)
library(ranger)

makeRLearner.regr.ranger = function() {
  makeRLearnerRegr(
    cl = "regr.ranger",
    package = "ranger",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num.trees", lower = 1L, default = 500L),
      # FIXME: Add default value when data dependent defaults are implemented: mtry=floor(sqrt(#independent vars))
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeIntegerLearnerParam(id = "min.node.size", lower = 1L, default = 5L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeNumericLearnerParam(id = "sample.fraction", lower = 0L, upper = 1L),
      makeNumericVectorLearnerParam(id = "split.select.weights", lower = 0, upper = 1),
      makeUntypedLearnerParam(id = "always.split.variables"),
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = TRUE),
      makeDiscreteLearnerParam(id = "importance", values = c("none", "impurity", "permutation"), default = "none", tunable = FALSE),
      makeLogicalLearnerParam(id = "write.forest", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "scale.permutation.importance", default = FALSE, requires = quote(importance == "permutation"), tunable = FALSE),
      makeIntegerLearnerParam(id = "num.threads", lower = 1L, when = "both", tunable = FALSE),
      makeLogicalLearnerParam(id = "save.memory", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, when = "both", tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", when = "both", tunable = FALSE),
      makeDiscreteLearnerParam(id = "splitrule", values = c("variance", "maxstat"), default = "variance"),
      makeNumericLearnerParam(id = "alpha", lower = 0L, upper = 1L, default = 0.5, requires = quote(splitrule == "maxstat")),
      makeNumericLearnerParam(id = "minprop", lower = 0L, upper = 1L, default = 0.1, requires = quote(splitrule == "maxstat")),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(num.threads = 1L, verbose = FALSE, respect.unordered.factors = TRUE),
    properties = c("numerics", "factors", "ordered", "featimp"),
    name = "Random Forests",
    short.name = "ranger",
    note = "By default, internal parallelization is switched off (`num.threads = 1`), `verbose` output is disabled, `respect.unordered.factors` is set to `TRUE`. All settings are changeable."
  )
}

#' @export
trainLearner.regr.ranger = function(.learner, .task, .subset, .weights, ...) {
  tn = getTaskTargetNames(.task)
  ranger::ranger(formula = NULL, dependent.variable = tn, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.regr.ranger = function(.learner, .model, .newdata, ...) {
  p = predict(object = .model$learner.model, data = .newdata, ...)
  return(p$predictions)
}

#' @export
getFeatureImportanceLearner.regr.ranger = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.ranger(.learner, .model, ...)
}

# create mlr train and test task
trainTask = makeRegrTask(data = as.data.frame(testTrain), target = "SalePrice")
testTask = makeRegrTask(data = as.data.frame(test[,2:78]), target = "SalePrice")

# Measures
m1 = rmse
m2 = setAggregation(rmse, train.rmse)

# specify mlr learner with some nice hyperpars
set.seed(123)
lrn = makeLearner("regr.ranger")
lrn = setHyperPars(lrn, 
                   num.trees = 200,
                   min.node.size = 5,
                   respect.unordered.factors = TRUE,
                   verbose = TRUE,
                   mtry = 5,
                   importance = "impurity"
)

# 1) make parameter set
ps = makeParamSet(
  # for RF, start with # of trees
  # then max tree depth
  # and minimum sample leaf
  makeIntegerParam("num.trees", lower = 300, upper = 500),
  #makeIntegerParam("min.node.size", lower = 1, upper = 8),
  #makeDiscreteParam("num.trees", values = c(200, 250, 500, 750, 1000)),
  makeLogicalParam("respect.unordered.factors", TRUE),
  makeDiscreteParam("importance", "impurity"),
  makeIntegerParam("mtry", lower = 12, upper = 24)
)

# 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 10L, predict = "both")

# 3) Here we use random search (with 5 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 5)


# 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters in parallel
#parallelStartMulticore(5)
#res = tuneParams(lrn, task = trainTask, resampling = rdesc,
#                 par.set = ps, control = ctrl)
res = tuneParams(lrn, 
                 task = trainTask, 
                 resampling = rdesc,
                 par.set = ps, 
                 control = makeTuneControlGrid(resolution = 10L),
                 measures = m1
                 )                 
opt.grid = as.data.frame(res$opt.path)
res

# Train on entire dataset (using best hyperparameters)
lrn = setHyperPars(lrn, par.vals = res$x)
mod = train(lrn, trainTask)

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
write.csv(submission,file = 'Submissions/ranger-mlr-v2-12-22-16.csv',row.names = FALSE)

