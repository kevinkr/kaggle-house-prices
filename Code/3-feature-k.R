###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature k
# Reduce zero, near zero variance
library(caret)
nzv_cols <- nearZeroVar(fullSet)
names(fullSet[nzv_cols])
if(length(nzv_cols) > 0)
  fullSet <- fullSet[, -nzv_cols]
