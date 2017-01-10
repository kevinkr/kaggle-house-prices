###########################################
# Kaggle Housing Prices
# Feature Engineering - 
# Feature i
# Skewness of continuous values
num.var <- names(fullSet)[which(sapply(fullSet, is.numeric))]
# determine skew for each numeric feature
library(moments)
skewed_feats <- sapply(num.var,function(x){skewness(fullSet[[x]],na.rm=TRUE)})

# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.80]
skewed_feats
# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  fullSet[[x]] <- log(fullSet[[x]] + 200)
}
