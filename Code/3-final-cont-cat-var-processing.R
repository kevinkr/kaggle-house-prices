# Kaggle Housing Prices
# Feature engineering
# Process cont and cat variables before splitting

# get var names for factors and numerics
#cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
#num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]

# Adjust skewness in continuous ---------------------------------

#first get data type for each feature
# cont_nums <- c("LotArea","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF", 
#                "TotalBsmtSF","FirstFlrSF","SecondFlrSF","LowQualFinSF","GrLivArea","GarageArea",
#                "WoodDeckSF","OpenPorchSF","EnclosedPorch","ThreeSsnPorch","ScreenPorch","PoolArea",
#                "MiscVal","newBsmtQualSF","newBsmtFinTypeSF")

# library(data.table)
# library(MASS)
# library(forecast)
# library(e1071)
# library(caret)
# library(forecast)

# fullSet <- setDT(fullSet)
# # remove skewness in train
# for (f in cont_nums) {
#   print(f)
#   tst <- e1071::skewness(fullSet[, eval(as.name(f))])
#   if (tst > .25) {
#     if (is.na(fullSet[, BoxCoxTrans(eval(as.name(f)))$lambda])) next
#     fullSet[, eval(as.name(f)) := BoxCox(eval(as.name(f)), BoxCoxTrans(eval(as.name(f)))$lambda)]
#   }
# }
# 
# # scale train
# for (f in cont_nums) {
#   fullSet[, eval(as.name(f)) := scale(eval(as.name(f)))]
# }
#feature_classes <- sapply(names(fullSet),function(x){class(fullSet[[x]])})
#numeric_feats <-names(feature_classes[feature_classes != "factor"])



#fullSet <- as.data.frame(fullSet)



# ################# Zero or near zero variance
# df = fullSet[,(names(fullSet) %in% cat.var)] # use fullSet for analysis
# x <- nearZeroVar(df, saveMetrics = TRUE)
# 
# x[x[,"zeroVar"] > 0, ]
# x[x[,"zeroVar"] + x[, "nzv"] > 0, ]
# 
# 

#7. Check for zero variance predictors in test data
# nzv_cols <- nearZeroVar(test.complete)
# names(test[nzv_cols])
# if(length(nzv_cols) > 0)
#   test.complete <- test.complete[, -nzv_cols] # 1459 61
# dim(test.complete)