# Kaggle Housing Prices
# Feature engineering
# Process cont and cat variables before splitting

# get var names for factors and numerics
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]

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
# determine skew for each numeric feature
library(moments)
skewed_feats <- sapply(num.var,function(x){skewness(fullSet[[x]],na.rm=TRUE)})

# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.80]

# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  fullSet[[x]] <- log(fullSet[[x]] + 1)
}


# Multicollinearity of numeric variables ---------------------------------------------
library(caret)
correlCutOff <- 0.90
#df = train[,(names(train) %in% num.var)]
#df = fullSet[,(names(fullSet) %in% num.var)] # use fullSet for analysis
df <- subset(fullSet, select = num.var)
df2 = fullSet[,!(names(fullSet) %in% num.var)]
descrCorr <- cor(df)
descrCorr <- cor(fullSet[,(names(fullSet) %in% num.var)])
highCorr <- findCorrelation(descrCorr, correlCutOff)
colnames(df[,highCorr])
# remove highly correlated  continuous variables
if (length(highCorr) > 0) {
  df <- df[, -highCorr]
  fullSet <- cbind(df2,df)
}

#fullSet <- as.data.frame(fullSet)
cat.var <- names(fullSet)[which(sapply(fullSet, is.factor))]
# Remove insignificant categorical variables --------------------
######
# category reduction fucntion
######
# inputs category name, cutoff value
reduce_cats <- function(cat.name, cutoff.val) {
  prop.table <- sort(prop.table(table(fullSet[[cat.name]])), decreasing = T)
  #return(proptable)
  weak.prop.table <- prop.table < cutoff.val
  #return(weak.prop.table)
  # grab the names
  weak.prop.names <- names(prop.table[prop.table < cutoff.val])
  return(weak.prop.names)
}

############full loop attempt
for (n in cat.var) {
  #print(n)
  # call function to return category names for reduction, number is cutoff val
  #weak.prop.names <- reduce_cats(cat.name, 0.01)
  weak.prop.names <- reduce_cats(n, 0.05)
  # filter data set by categories that are in the weak prop names vector using %in% search'
  # first convert to character
  fullSet[[n]] <- as.character(fullSet[[n]])
  fullSet[fullSet[[n]] %in% weak.prop.names, n] <- "OTHER"
  fullSet[[n]] <- as.factor(fullSet[[n]])
}


# ################# Zero or near zero variance
# df = fullSet[,(names(fullSet) %in% cat.var)] # use fullSet for analysis
# x <- nearZeroVar(df, saveMetrics = TRUE)
# 
# x[x[,"zeroVar"] > 0, ]
# x[x[,"zeroVar"] + x[, "nzv"] > 0, ]
# 
# 
nzv_cols <- nearZeroVar(fullSet)
names(fullSet[nzv_cols])
if(length(nzv_cols) > 0)
  fullSet <- fullSet[, -nzv_cols] # 1460 60
dim(fullSet)

#7. Check for zero variance predictors in test data
# nzv_cols <- nearZeroVar(test.complete)
# names(test[nzv_cols])
# if(length(nzv_cols) > 0)
#   test.complete <- test.complete[, -nzv_cols] # 1459 61
# dim(test.complete)