# Kaggle House Prices 12-21-16
# MLR Ranger

library(mlbench)
library(ranger)
library(Metrics)
library(mlr)


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
      makeLogicalLearnerParam(id = "respect.unordered.factors", default = FALSE),
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
testTask = makeRegrTask(data = as.data.frame(subset(test, select = c(-Id))), target = "SalePrice")

# Measures
m1 = rmse
m2 = setAggregation(rmse, train.rmse) # unload Metrics package if error

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
  makeIntegerParam("num.trees", lower = 20, upper = 100),
  #makeIntegerParam("min.node.size", lower = 1, upper = 8),
  #makeDiscreteParam("num.trees", values = c(200, 250, 500, 750, 1000)),
  makeIntegerParam("mtry", lower = 5, upper = 20)
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
                 control = makeTuneControlGrid(resolution = 5L),
                 measures = list(m1, m2)
                 )                 

# Train on entire dataset (using best hyperparameters)
lrn = setHyperPars(lrn, par.vals = res$x)
mod = train(lrn, trainTask)

predict = predict(mod, trainTask)
predict
rmse(log(testTrain$SalePrice),log(as.data.frame(predict)))

# predict on new data
predict = predict(mod, newdata = validTrain)
predict
rmse(log(validTrain$SalePrice),log(as.data.frame(predict)))

#######################
opt.grid = as.data.frame(res$opt.path)
g = ggplot(opt.grid, aes(x = num.trees, y = mtry, fill = rmse.test.rmse))
g + geom_tile()

res_data = generateHyperParsEffectData(res)
ggplot(data=res_data$data, aes(x=mtry, y=rmse.train.rmse)) + 
  geom_line(aes(color="rmse.train.rmse")) + 
  geom_line(aes(y=rmse.test.rmse, color = "rmse.test.rmse")) +
  facet_wrap(~num.trees)

# mtry
# Let's explore various training set sizes for each 
lrn_best = setHyperPars(makeLearner('regr.ranger', id = "opt_regr.ranger"), par.vals = res$x)
lrn_max1 = setHyperPars(makeLearner('regr.ranger', id= "mtry = 11"), par.vals = list(mtry = 11))
lrn_max5 = setHyperPars(makeLearner('regr.ranger', id= "mtry = 17"), par.vals = list(mtry = 17))
lrn_max10 = setHyperPars(makeLearner('regr.ranger', id= "mtry = 22"), par.vals = list(mtry = 22))

r = generateLearningCurveData(list(lrn_best, lrn_max1, lrn_max5, lrn_max10, 'regr.ranger'), 
                              task = trainTask,
                              percs = seq(0.1, 1, by = 0.1),
                              measures = list(m1, m2),
                              show.info = TRUE,
                              resampling = rdesc
)
plotLearningCurve(r, facet = "learner", pretty.names = FALSE)
plotLearningCurve(r, pretty.names = FALSE)

# num.trees
# Let's explore various training set sizes for each 
lrn_best = setHyperPars(makeLearner('regr.ranger', id = "opt_regr.ranger"), par.vals = res$x)
lrn_max1 = setHyperPars(makeLearner('regr.ranger', id= "num.trees = 10"), par.vals = list(num.trees = 10))
lrn_max5 = setHyperPars(makeLearner('regr.ranger', id= "num.trees = 200"), par.vals = list(num.trees = 200))
lrn_max10 = setHyperPars(makeLearner('regr.ranger', id= "num.trees = 500"), par.vals = list(num.trees = 500))

r = generateLearningCurveData(list(lrn_best, lrn_max1, lrn_max5, lrn_max10, 'regr.ranger'), 
                              task = trainTask,
                              percs = seq(0.1, 1, by = 0.1),
                              measures = list(m1, m2),
                              show.info = TRUE,
                              resampling = rdesc
)
plotLearningCurve(r, facet = "learner", pretty.names = FALSE)
plotLearningCurve(r, pretty.names = FALSE)

# best parameters
res$x
# test result
res$y

# resampling
r = resample("regr.ranger", trainTask, rdesc,
             measures = m1, par.vals = res$x, show.info = FALSE)
r$aggr
r$measures.train
r$measures.test

##############################################


# train on full trian set
fullTrainTask = makeRegrTask(data = as.data.frame(train), target = "SalePrice")
final_mod = train(lrn, fullTrainTask)

pred = getPredictionResponse(predict(final_mod, testTask))
summary(pred)

SUBMISSION_FILE = "Data/sample_submission.csv"
library(data.table)
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$SalePrice = pred
write.csv(submission,file = 'Submissions/ranger-mlr-v2-12-22-16.csv',row.names = FALSE)

