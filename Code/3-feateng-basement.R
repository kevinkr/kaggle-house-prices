# Kaggle Housing Prices
# Feature engineering
# use 4-data-prep to split into train and test

# Examine Basement variables using RF to identify important variables
library(mlr)
library(mlbench)
library(ranger)

# create df
basementTrain <- subset(train, select = grepl("Bsmt", names(train)))
basementTrain <- cbind(basementTrain,SalePrice=train$SalePrice)

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
trainTask = makeRegrTask(data = basementTrain, target = "SalePrice")
#testTask = makeRegrTask(data = as.data.frame(test[,2:78]), target = "SalePrice")

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
  makeIntegerParam("num.trees", lower = 1, upper = 500),
  #makeIntegerParam("min.node.size", lower = 1, upper = 8),
  #makeDiscreteParam("num.trees", values = c(200, 250, 500, 750, 1000)),
  makeIntegerParam("mtry", lower = 1, upper = 9)
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
opt.grid = as.data.frame(res$opt.path)
g = ggplot(opt.grid, aes(x = num.trees, y = mtry, fill = rmse.test.rmse))
g + geom_tile()
library(FSelector)
fv = generateFilterValuesData(trainTask, method = "information.gain")
fv
fv2 = generateFilterValuesData(trainTask, method = c("information.gain", "chi.squared"))
fv2
plotFilterValues(fv2)
plotFilterValuesGGVIS(fv2)

# Bsmt Quality
levels(fullSet$BsmtQual)
table(fullSet$BsmtQual)
fullSet$newBsmtQualSF <- as.numeric(factor(fullSet$BsmtQual, levels=c("No Bsmnt", "Po",
                                                  "Fa", "TA", "Gd", "Ex")))
head(fullSet[,c("BsmtQual","newBsmtQualSF")])
fullSet$newBsmtQualSF <- fullSet$newBsmtQualSF * fullSet$TotalBsmtSF

# Bsmt Fin Type 1
levels(fullSet$BsmtFinType1)
fullSet$newBsmtFinTypeSF <- as.numeric(factor(fullSet$BsmtFinType1, levels=c("No Bsmnt", "Unf",
                                                                      "LWQ", "Rec", "BLQ", "ALQ", "GLQ")))
head(fullSet[,c("BsmtFinType1","newBsmtFinTypeSF")])
fullSet$newBsmtFinTypeSF <- fullSet$newBsmtFinTypeSF * fullSet$BsmtFinSF1
