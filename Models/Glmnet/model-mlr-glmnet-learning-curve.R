# Kaggle House Prices 12-26-16
# MLR GLMNet

library(mlbench)
library(glmnet)
library(Metrics)
library(mlr)

#' @export
makeRLearner.regr.glmnet = function() {
  makeRLearnerRegr(
    cl = "regr.glmnet",
    package = "glmnet",
    par.set = makeParamSet(
      makeDiscreteLearnerParam(id = "family", values = c("gaussian", "poisson"), default = "gaussian"),
      makeNumericLearnerParam(id = "alpha", default = 1, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "s", lower = 0, when = "predict"),
      makeLogicalLearnerParam(id = "exact", default = FALSE, when = "predict"),
      makeIntegerLearnerParam(id = "nlambda", default = 100L, lower = 1L),
      makeNumericLearnerParam(id = "lambda.min.ratio", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lambda", lower = 0),
      makeLogicalLearnerParam(id = "standardize", default = TRUE),
      makeLogicalLearnerParam(id = "intercept", default = TRUE),
      makeNumericLearnerParam(id = "thresh", default = 1e-07, lower = 0),
      makeIntegerLearnerParam(id = "dfmax", lower = 0L),
      makeIntegerLearnerParam(id = "pmax", lower = 0L),
      makeIntegerVectorLearnerParam(id = "exclude", lower = 1L),
      makeNumericVectorLearnerParam(id = "penalty.factor", lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "lower.limits", upper = 0),
      makeNumericVectorLearnerParam(id = "upper.limits", lower = 0),
      makeIntegerLearnerParam(id = "maxit", default = 100000L, lower = 1L),
      makeDiscreteLearnerParam(id = "type.gaussian", values = c("covariance","naive")),
      makeLogicalLearnerParam(id = "standardize.response", default = FALSE),
      makeNumericLearnerParam(id = "fdev", default = 1.0e-5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "devmax", default = 0.999, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "eps", default = 1.0e-6, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "big", default = 9.9e35),
      makeIntegerLearnerParam(id = "mnlam", default = 5, lower = 1),
      makeNumericLearnerParam(id = "pmin", default = 1.0e-9, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "exmx", default = 250),
      makeNumericLearnerParam(id = "prec", default = 1e-10),
      makeIntegerLearnerParam(id = "mxit", default = 100L, lower = 1L),
      makeLogicalLearnerParam(id = "factory", default = FALSE)
    ),
    properties = c("numerics", "factors", "ordered", "weights"),
    par.vals = list(s = 0.01),
    name = "GLM with Lasso or Elasticnet Regularization",
    short.name = "glmnet",
    note = "Factors automatically get converted to dummy columns, ordered factors to integer.
    Parameter `s` (value of the regularization parameter used for predictions) is set to `0.1` by default,
    but needs to be tuned by the user."
  )
}

#' @export
trainLearner.regr.glmnet = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  info = getFixDataInfo(d$data, factors.to.dummies = TRUE, ordered.to.int = TRUE)
  args = c(list(x = as.matrix(fixDataForLearner(d$data, info)), y = d$target), list(...))
  rm(d)
  if (!is.null(.weights))
    args$weights = .weights
  
  saved.ctrl = glmnet::glmnet.control()
  is.ctrl.arg = names(args) %in% names(saved.ctrl)
  if (any(is.ctrl.arg)) {
    on.exit(do.call(glmnet::glmnet.control, saved.ctrl))
    do.call(glmnet::glmnet.control, args[is.ctrl.arg])
    args = args[!is.ctrl.arg]
  }
  
  attachTrainingInfo(do.call(glmnet::glmnet, args), info)
}

#' @export
predictLearner.regr.glmnet = function(.learner, .model, .newdata, ...) {
  info = getTrainingInfo(.model)
  .newdata = as.matrix(fixDataForLearner(.newdata, info))
  drop(predict(.model$learner.model, newx = .newdata, ...))
}

# create mlr train and test task
trainTask = makeRegrTask(data = as.data.frame(testTrain), target = "SalePrice")
testTask = makeRegrTask(data = as.data.frame(subset(test, select = c(-Id))), target = "SalePrice")

# Measures
m1 = rmse
m2 = setAggregation(rmse, train.rmse) # unload Metrics package if error

# specify mlr learner with some nice hyperpars
set.seed(123)
lrn = makeLearner("regr.glmnet")
lrn$par.set
#lrn = setHyperPars(lrn, 
#                    min.node.size = 5,
#                    respect.unordered.factors = TRUE,
#                    verbose = TRUE,
#                    mtry = 5,
#                    importance = "impurity"
#)

# 1) make parameter set
ps = makeParamSet(
  makeNumericParam("s", lower = 0, upper = .005),
  makeNumericParam("alpha", lower = 0, upper = .05)
)

# 2) Use 3-fold Cross-Validation to measure improvements
rdesc = makeResampleDesc("CV", iters = 10L, predict = "both")

# 3) Here we use random search (with 5 Iterations) to find the optimal hyperparameter
ctrl =  makeTuneControlRandom(maxit = 5)

# 4) now use the learner on the training Task with the 3-fold CV to optimize your set of parameters in parallel
#parallelStartMulticore(5)
#res = tuneParams(lrn, task = trainTask, resampling = rdesc,
#                 par.set = ps, control = ctrl)
# alpha = 1 is lasso model
# alpha = 0 is ridge model

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
g = ggplot(opt.grid, aes(x = s, y = alpha, fill = rmse.test.rmse))
g + geom_tile()

res_data = generateHyperParsEffectData(res)
ggplot(data=res_data$data, aes(x=s, y=rmse.train.rmse)) + 
  geom_line(aes(color="rmse.train.rmse")) + 
  geom_line(aes(y=rmse.test.rmse, color = "rmse.test.rmse")) +
  facet_wrap(~alpha)

# s
# Let's explore various training set sizes for each 
lrn_best = setHyperPars(makeLearner('regr.glmnet', id = "opt_regr.glmnet"), par.vals = res$x)
lrn_max1 = setHyperPars(makeLearner('regr.glmnet', id= "s = .001"), par.vals = list(s = .001))
lrn_max5 = setHyperPars(makeLearner('regr.glmnet', id= "s = .05"), par.vals = list(s = .05))
lrn_max10 = setHyperPars(makeLearner('regr.glmnet', id= "s = .1"), par.vals = list(s = .1))

r = generateLearningCurveData(list(lrn_best, lrn_max1, lrn_max5, lrn_max10, 'regr.glmnet'), 
                              task = trainTask,
                              percs = seq(0.1, 1, by = 0.1),
                              measures = list(m1, m2),
                              show.info = TRUE,
                              resampling = rdesc
)
plotLearningCurve(r, facet = "learner", pretty.names = FALSE)
plotLearningCurve(r, pretty.names = FALSE)

# alpha
# Let's explore various training set sizes for each 
lrn_best = setHyperPars(makeLearner('regr.glmnet', id = "opt_regr.glmnet"), par.vals = res$x)
lrn_max1 = setHyperPars(makeLearner('regr.glmnet', id= "alpha = 0"), par.vals = list(alpha = 0))
lrn_max5 = setHyperPars(makeLearner('regr.glmnet', id= "alpha = .5"), par.vals = list(alpha = 0.5))
lrn_max10 = setHyperPars(makeLearner('regr.glmnet', id= "alpha = 1"), par.vals = list(alpha = 1.0))

r = generateLearningCurveData(list(lrn_best, lrn_max1, lrn_max5, lrn_max10, 'regr.glmnet'), 
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
r = resample("regr.glmnet", trainTask, rdesc,
             measures = m1, par.vals = res$x, show.info = FALSE)
r$aggr
r$measures.train
r$measures.test
# r
# rmse(log(r$pred$data$truth),log(r$pred$data$response))
# with( r$pred$data, table(truth, response) )
##############################################


# train on full trian set
fullTrainTask = makeRegrTask(data = as.data.frame(train), target = "SalePrice")
final_mod = train(lrn, fullTrainTask)

pred = getPredictionResponse(predict(final_mod, testTask))
summary(pred)

SUBMISSION_FILE = "Data/sample_submission.csv"
submission = fread(SUBMISSION_FILE, colClasses = c("integer", "numeric"))
submission$SalePrice = pred
write.csv(submission,file = 'Submissions/glmnet-mlr-v1-12-26-16.csv',row.names = FALSE)

